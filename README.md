# InteractiveNJ
# NJ Transit & GLAM Accessibility Dashboard

This Shiny dashboard explores public transit stops and cultural institutions (GLAMs) across New Jersey. It includes an interactive map, searchable data tables, and demographic plots using ACS data.

---

## Census API Key Setup

To regenerate ACS-based data (e.g., population), you’ll need a Census API key:

1. Request a key from [https://api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)
2. In R, run:
   ```r
   tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)
Restart R to activate the key. The app will read it from your .Renviron file.

File Overview
census_data_processor.R — Main Shiny app script

NJ_data_clean.RData — Pre-processed spatial and census data

census_data_module.R — Functions for census data (income, age/gender)

transit_trends.R — Transit-related plots and analysis

README.md — Project overview and instructions

Data Sources
To regenerate NJ_data_clean.RData, ensure the following files are in the same folder as load_data.R:

Monmouth_County_Historic_Sites_Inventory_PV.csv

geocoded_spreadsheet.csv

nj_higher_ed_geocoded.csv

NJCounties.shp and associated shapefile components

RMHA-map-2.geojson

The script also pulls live data from:

NJ Transit GTFS feeds (rail and bus)

ArcGIS REST API for libraries

ACS 2021 via tidycensus (requires Census API key)

⚙️ Requirements
R version ≥ 4.2

Required packages:

r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "sf", "dplyr",
  "DT", "RColorBrewer", "ggplot2", "purrr", "stringr", 
))
🖥 Dashboard Features
🗺 Interactive Map
View institutions by category and transit stops

Filter by county, institution type, and transit type

Click markers for details

Value Boxes
Total transit stops

Total libraries

Total institutions
Data Table
Searchable and sortable list of institutions
Plots
Median household income by county

Population by county

Age and gender distribution over time

Transit and motorcycle trends

Notes
Clear/Deselect buttons reset filters and map markers

Data is pre-processed (NJ_data_clean.RData) to improve performance

All scripts must be in the same folder for the app to run correctly. 
How to Run the App
Place all files in the same folder.

Open census_data_processor.R in RStudio.

Run the app:

r
shiny::runApp()
The dashboard will launch in your default browser.
