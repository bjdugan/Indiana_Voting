# Indiana_Voting
Facts, figures, and data on voting and vote center adoption in Indiana

View the [dashboard](https://0196e88f-48c5-a917-025f-0c6d9f0594af.share.connect.posit.cloud/)

## Overview
This project contains several R files and datasets:  
 - **import.R** retrieves and cleans data from sources listed below.  
 - **IN_counties.geojson** spatial data for mapping Indiana counties.  
 - **registration_turnout2010-2024.csv** data scraped from PDFs for Indiana general elections beginning in 2010. Municipal election data are only available for recent years, and pre-2010 data do not appear to be available. Election year, turnout and absenteee turnout rates, and counts of registered, voting, and absentee voters are included.  
 - **vote_center_adoption.csv** table read from IN Secretary of State website that includes the county and year vote centers were adopted over traditional precinct-based voting.  
 - **county_types.csv** (Not currently used) data from the US Department of Agriculture's Economic Research Service that describe counties based on prevalent industry and a variety of sociodemographic factors.  
 
## Background
In 2024, Monroe County, Indiana, began a study to evaluate the costs and logistics of adopting vote centers within the county and seeks public feedback through May 19th, 2025. In late April, 2025, Indiana Governor Mike Braun signed legislation that may eventually result in all Indiana counties adopting vote centers. This project uses publicly available voter data to explore trends and claims regarding improved access. 

## Sources
### Data
 - Voter registration and turnout [statistics](https://www.in.gov/sos/elections/voter-information/register-to-vote/voter-registration-and-turnout-statistics/)  
 - Indiana Vote Center adoption [data](https://www.in.gov/sos/elections/voter-information/ways-to-vote/vote-centers/)  
 - [FAQ](https://www.in.gov/sos/elections/voter-information/ways-to-vote/vote-centers/vote-center-information/) and background about Vote Centers in Indiana  
 - Spatial data [API](https://www.indianamap.org/datasets/INMap::county-boundaries-of-indiana-current/about)  
 - USDA ERS County [Typologies](https://www.ers.usda.gov/data-products/county-typology-codes) (not used outside exploratory analysis)  

### News
 - B Square Bulletin local [coverage](https://bsquarebulletin.com/april-7-public-hearing-kicks-off-formal-feedback-period-on-vote-centers-for-monroe-county/) of public hearing in Monroe County for vote center adoption  
 - Indiana Daily Student local [coverage](https://www.idsnews.com/article/2025/04/monroe-county-vote-center-council-election) on late April update to Monroe County Council  
 - Monroe County Vote Center Study Committee [recommendations](https://bloomdocs.org/wp-content/uploads/simple-file-list/2025-03-06-VCSC-Report-Final-Draft-7_4-Apdx2.pdf)  
 - [House Bill](https://iga.in.gov/legislative/2025/bills/house/1633/details) 1633 requiring study of all counties using vote centers, signed into law 2025-04-22  