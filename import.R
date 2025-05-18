# importing data from various sources
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readxl)
library(pdftools) # reading from pdfs
library(rvest) # pulling from webpages
library(ggplot2)
library(sf) # reading json from api

# IN Vote Center Adoption by county and year ####
# avoid subsequent requests to be polite
if (!file.exists("data/vote_center_adoption.csv")) {
  vote_centers <- "https://www.in.gov/sos/elections/voter-information/ways-to-vote/vote-centers/" |> 
    read_html() |> 
    html_table(header = TRUE) |> 
    map(select, county = County, adoption_year = `Date Adopted`) |> 
    pluck(1)
} else {
  cat("Already have data, loading.")
  vote_centers <- read.csv("data/vote_center_adoption.csv")
}

# complete list of Indiana counties ####
# from https://www.indianamap.org/datasets/INMap::county-boundaries-of-indiana-current/about
if (!file.exists("data/IN_counties.geojson")) {
  IN_counties <- sf::st_read("https://gisdata.in.gov/server/rest/services/Hosted/County_Boundaries_of_Indiana_Current/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
    select(-name_lcase, -name_ucase)
} else {
  cat("Already have data, loading")
  IN_counties <- st_read("data/IN_counties.geojson")
}

# head(IN_counties)
# 
# ggplot(IN_counties) + 
#   geom_sf()

# grabbing turnout data from PDFs ####
# retrieved from https://www.in.gov/sos/elections/voter-information/register-to-vote/voter-registration-and-turnout-statistics/
# from 2010 to 2018 reports were generally titled {year}_General_Election_Turnout_[Data|Report], 
# 2020 was Election_Turnout_and_Registration_{datetimestamp}, and 2022 was Eelction_Turnout_and_Registration_2022_general
# 2024 was called Turnout and Regstration Report.

election_years <- seq(2010, 2024, by = 2)
pres_election_years <- seq(2012, 2024, by = 4)

reg_turnout <- tibble(pdf = dir("data", pattern = "Turnout", full.names = TRUE)) |> 
  mutate(election_year = as.integer(str_extract(pdf, "\\d{1,4}")),
         data = map(pdf, pdf_text) |> 
           map(paste, collapse = "") |>  # combine multiple pages into one
           str_split("\n"),
         pres_election_year = if_else(election_year %in% pres_election_years, 
                                      "Presidential elections", 
                                      "Mid-term elections")) |> 
  arrange(election_year)

# having some familiarity with the table will help. Generally,
# 1st line is title, repeated on each page
# 2nd line is date in format FORMAT of election
# 3rd line is ignorable table title
# actual table starts at line 5
# total line is very last
# unnest(reg_turnout[1, ], "data") |> 
#   pull(data) |>
#   cat(sep = "\n")
# HOWEVER 2018, 2018-General-Election-Turnout-from-the-ENR-page.pdf, appears to be a screenshot or have a different format
# e.g., line 2 includes column headers but col 2's header is split into row 1 as well ("Registered\nVoters")
# it otherwise has the same cols: county, registered (voters), (voters) voting, 
# turnout(_rate), election day (voting_in_person), (voting_)absentee, *(voting_)abseentee(_rate)
# also note the "totals" is included but oddly offset one row
#reg_turnout[5, ]$data

reg_turnout18 <- filter(reg_turnout, election_year == 2018)
reg_turnout <- filter(reg_turnout, election_year != 2018)

reg_turnout <- mutate(
  reg_turnout,
  # extract year and type of election
  election_year2 = map(data, pluck, 1) |> 
    str_extract("^\\d{1,4}") |> 
    as.integer(),
  # extract voting type (general etc)
  election_type = map(data, pluck, 1) |> 
    str_remove("\\d*") |> 
    str_squish(),
  # extract voting day
  election_date = map_chr(data, \(x) unique(grep("^Tuesday", x, value = TRUE))),
  # remove table header, "{Voting type} Turnout and Registration
  data = map(data, str_remove_all, "\\s*General Election Turnout and Registration") |> 
    map(str_remove_all, election_date) |> 
    map(str_remove_all, paste(election_year2, election_type)) |> 
    # any page number stuff
    map(str_remove_all, "Page \\d of \\d") |> 
    # 2024's has an apparent production date
    map(str_remove_all, "\\s*[A-z]+, [A-z]+ \\d{1,2}, \\d{4}") |> 
    # remove commas and percentage marks
    map(str_remove_all, ",|%") |> 
    map(str_squish) |> 
    map(discard, \(x) nchar(x) == 0),
  # grab colnames 
  cn = map(data, pluck, 1),
  # and remove those rows
  data = map(data, discard, \(x) x == cn) |> 
    # add "total" label to our last total row
    map(map, \(x) if (!grepl("^[A-Z]", x)) paste("Total", x) else x) |> 
    # fix any oddities in naming like hyphens or dashes - can be fixed later
    map(str_replace, "\\.\\s|\\-", "") |> 
    map(str_replace, "La Porte", "LaPorte"),
  # then get the table into shape...
  data = map(data, str_split, "\\s") |>
    map(list_flatten) |>
    map(map, set_names, c("county", "registered", "voting", "turnout_rate",
                          "voting_in_person", "voting_absentee", 
                          "voting_absentee_rate")) |>
    # a little hacky but faster than pivot wider
    map(map, data.frame) |>
    map(map, t) |>
    map(map, data.frame) |>
    map(list_rbind) |>
    map(as_tibble),
  .by = election_year
) |> 
  arrange(election_year) 

head(reg_turnout)

unnest(reg_turnout, data)

# handle 2018 not having some elements and not needing others
reg_turnout18 <- mutate(
  reg_turnout18, 
  # extract year and type of election, mainly as check
  election_year2 = 2018L,
  # extract voting type (general etc)
  election_type = "General Election",
  # extract voting day - # https://en.wikipedia.org/wiki/2018_United_States_elections
  election_date = "Tuesday, November 6, 2018", 
  # colname "Registered Voters" appears split between rows, as "registered" and "voters"
  data = map(data, `[`, -1),
  # remove commas and percentage marks
  data = map(data, str_remove_all, ",|%") |> 
    map(str_squish) |> 
    map(discard, \(x) nchar(x) == 0),
  # grab colnames for eventual table...or disregard
  cn = map(data, pluck, 1),
  # and remove those rows
  data = map(data, discard, \(x) x == cn) |> 
    # add "total" label to our last total row
    map(map, \(x) if (!grepl("^[A-Z]", x)) paste("Total", x) else x) |> 
    # fix any oddities in naming like hyphens or dashes - can be fixed later
    map(str_replace, "\\.\\s|\\-", "") |> 
    map(str_replace, "La Porte", "LaPorte"),
  # drop empty "total" row
  data = map(data, discard, \(x) x == "Totals"),
  # then get the table into shape...
  data = map(data, str_split, "\\s") |>
    map(list_flatten) |>
    map(map, set_names, c("county", "registered", "voting", "turnout_rate",
                          "voting_in_person", "voting_absentee", 
                          "voting_absentee_rate")) |>
    # a little hacky but faster than pivot wider
    map(map, data.frame) |>
    map(map, t) |>
    map(map, data.frame) |>
    map(list_rbind) |>
    map(as_tibble),
) |> 
  arrange(election_year) 

reg_turnout <- bind_rows(reg_turnout, reg_turnout18) |> 
  arrange(election_year) |> 
  select(-cn, -pdf, -election_type, -election_year2) |> 
  unnest(data) |> 
  mutate(across(registered:voting_absentee_rate, as.integer))

# County Typology Codes (US Dept. of Agriculture Economic Research Service)
# NOT USED outside EDA, consider omitting
# https://www.ers.usda.gov/data-products/county-typology-codes,
# given some of the descriptions for variables of interest (population loss, employment, postsecondary ed) amkes sense to jsut use 2025

if (!file.exists("data/county_types.csv")) {
  county_types <- read_xlsx("data/erscountytypology2025edition.xlsx") |> 
    rename_with(tolower) |> 
    filter(state == "IN") |> 
    rename(county = county_name) |> 
    mutate(county = str_remove(county, " County")) |> 
    mutate(
      #"The mutually exclusive ‘Industry Dependence’ Economic Typology Code is a single code that indicates if any of the industries examined have a high combined share of earnings and jobs, and which industry is the most prevalent in a county relative to the other industries. This code is more reflective of nonmetro counties’ industrial structures....This code indicates which of the five industries is the most dominant in a county."
      # and, filter(county_types_codebooks, variable_name == "Industry_Dependence_2025") |> pull(description)
      industry_dependence_fct = factor(industry_dependence_2025, 
                                        levels = 0:5,
                                        labels = c(
                                          "Not dependent", "Farming", "Mining",
                                          "Manufacturing", "Government", 
                                          "Recreation")))
  
  county_types_codebooks <- read_xlsx("data/erscountytypology2025edition.xlsx", 
                                      sheet = 2, skip = 1) |> 
    select(variable_name = 1, description = 2)
  
  list(county_types, county_types_codebooks) |> 
    set_names(paste0(c("data/county_types", "data/county_types_codebook"), ".csv")) |> 
    imap(\(x, y) write.csv(x, file = y, row.names = FALSE))
} else {
  county_types <- read.csv("data/county_types.csv") |> 
    as_tibble()
  county_types_codebook <- read.csv("data/county_types_codebook.csv") |> 
    as_tibble()
}

# what's Monroe like? Part of a Metro statistical area, high-government (state and fed, i.e., IU), dependence code is 4 (gov't), and housing stress.
filter(county_types, county == "Monroe") |> 
  glimpse()

# checks ####
# basic checks. Looks like LaPorte is sometimes recorded as "La Porte" instead - fixed above.
count(reg_turnout, county) |> 
  filter(n != length(election_years))

# check that each county has a match across datasets geospatial data, fix upon export
#De Kalb, La Porte, St Joseph, but the first two appear not to be listed with spaces elsewhere
select(IN_counties, name) |> 
  anti_join(
    distinct(reg_turnout, county), 
    by = c("name" = "county"))
# with spaces removed
distinct(reg_turnout, county) |> 
  anti_join(select(IN_counties, name), 
            by = c("county" = "name"))
# is St. Joseph (proper)
select(vote_centers, county) |> 
  anti_join(distinct(reg_turnout, county), by = "county")

# check that rates are more or less accurate
# no rate should be over 100, but Decatur's is (and is in the PDF)
select(reg_turnout, county, election_year, contains("rate")) |> 
  pivot_longer(-c(county, election_year)) |> 
  filter(value >= 100)
# again, decatur and ohio counties are kind of odd
select(reg_turnout, county, election_year, voting, voting_in_person) |> 
  mutate(voting_in_person_rate = voting_in_person / voting * 100) |> 
  filter(voting_in_person_rate >= 100)

# I would be surpised if absentee rates (except 2020, maybe 2022) were the majority. Note this does include early voting.
# 2020 had a much higher rate as expected, but 2016 saw an increase, and a similarly high rate in 2024, where still half of all counties had absentee rate just over 50% 
ggplot(filter(reg_turnout, voting_absentee_rate <= 100),
       aes(x = election_year, y = voting_absentee_rate, group = election_year)) + 
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(width = .2, height = 0) + 
  scale_x_continuous(breaks = election_years) + 
  scale_y_continuous(breaks = seq(0, 100, 25)) + 
  theme_minimal()

# there's a handful fo small cases not matching voter totals, but some are alarmingly high like Decatur 2022 or Ohio 2014, off by thousands.
# only my calculation of voting ("Voters Voting") doesn't match stated figures
# negative values indicate vote + absentee are greater than voters voting, which could mean there that the tallies are not final but voters voting is?
# could possibly be from provisional votes, but again I'd think these woudl be the final tallies.
reg_turnout_errors <- mutate(reg_turnout, 
       turnout_rate2 = round(voting / registered * 100), 
       turnout_match = turnout_rate - turnout_rate2,
       voting2 = voting_in_person + voting_absentee, 
       voting_match = voting - voting2,
       voting_absentee_rate2 = round(voting_absentee / voting * 100),
       voting_absentee_rate_match = round(voting_absentee_rate - voting_absentee_rate2)) |> 
  select(county, election_year, contains("match")) |> 
  pivot_longer(-c(county, election_year)) |> 
  filter(value != 0 & county != "Total") |> 
  arrange(desc(abs(value))) 

print(reg_turnout_errors)

# some could simply be the result of data entry error:
#  e.g. Monroe in 2010, someone could have misread 36633 as 36333
#  or Cass or Wells (each off by exactly 100), someone could have typed the adjacent key
#  and Dubois 2014, off by 540, someone could have switched positions of the 2 and 8, getting 11_2_87 instead of 11_8_27.
#  the even-hundred numbers are probably more suspect than others, and earlier tallies too.
#  it might be OK to switch these few cases but since I can't explain the others (and these coudl be by chance), shoudl just leave it and make note.
left_join(reg_turnout_errors, reg_turnout, by = c("county", "election_year")) |> 
  mutate(voting2 = voting_in_person + voting_absentee) |> 
  select(county, election_year, value, voting_in_person, voting_absentee, voting, voting2)

# noticed that Decatur's 2022 data were quite wonky: voting < voting_in_person and < voting_absentee
# even if they had swapped some fields, like absentee and voting, it woudlnt' add up
filter(reg_turnout, county == "Decatur" & election_year == 2022) |> 
  transmute(voting_in_person, voting_absentee, voting, voting2 = voting + voting_in_person)

# export #### 
mutate(reg_turnout, 
       county = if_else(county == "StJoseph", "St. Joseph", county)) |> 
  write.csv("data/registration_turnout2010-2024.csv", row.names = FALSE)

write.csv(vote_centers, "data/vote_center_adoption.csv", row.names = FALSE)

# adjust St. Joseph, others
mutate(IN_counties, name = case_when(name == "St Joseph" ~ "St. Joseph", 
                          name == "De Kalb" ~ "DeKalb", 
                          name == "La Porte" ~ "LaPorte", 
                          TRUE ~ name)) |> 
  # says it can't be appended
  st_write("data/IN_counties.geojson", append = FALSE) 
