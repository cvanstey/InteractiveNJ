# 🗺️ InteractiveNJ
## NJ Transit & GLAM Accessibility Dashboard

This **Shiny dashboard** explores **public transit stops** and **cultural institutions (GLAMs)** across New Jersey.  
It features an **interactive map**, **searchable data tables**, and **demographic visualizations** using U.S. Census ACS data.

---

## 🚀 Getting Started

### 1. Census API Key Setup

To regenerate ACS-based data (e.g., population, income, demographics), you’ll need a **Census API key**.

1. Request a key here:  
   👉 [https://api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)

2. In R, run:

   ```r
   tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)

   Restart R to activate the key.
The app will automatically read it from your .Renviron file.

📁 File Overview
File	Description
census_data_processor.R	Main Shiny app script
NJ_data_clean.RData	Pre-processed spatial and census data
census_data_module.R	Functions for ACS data (income, age/gender)
transit_trends.R	Transit-related plots and analysis
load_data.R	Loads and prepares all required datasets
README.md	Project documentation (this file)
🗂 Data Sources

To regenerate NJ_data_clean.RData, ensure the following files are in the same folder as load_data.R:

Monmouth_County_Historic_Sites_Inventory_PV.csv

geocoded_spreadsheet.csv

nj_higher_ed_geocoded.csv

NJCounties.shp and all related shapefile components

RMHA-map-2.geojson

The app also pulls live data from:

NJ Transit GTFS feeds (rail and bus)

ArcGIS REST API for library locations

ACS 2021 (tidycensus) — requires Census API key

⚙️ Requirements

R version: ≥ 4.2

Required packages:

install.packages(c(
  "shiny", "shinydashboard", "leaflet", "sf", "dplyr",
  "DT", "RColorBrewer", "ggplot2", "purrr", "stringr"
))

💡 Dashboard Features
🗺 Interactive Map

Toggle between institutions by category and transit stops

Filter by county, institution type, or transit type

Click markers for details and data source links

📊 Value Boxes

Total transit stops

Total libraries

Total institutions

📋 Data Table

Searchable and sortable list of all institutions

📈 Plots & Trends

Median household income by county

Population by county

Age and gender distribution over time

Transit and motorcycle trends

🧭 Notes

Clear/Deselect buttons reset filters and map markers

Data is pre-processed in NJ_data_clean.RData for performance

All scripts must be in the same folder for the app to run correctly

▶️ How to Run the App

Place all project files in the same directory

Open census_data_processor.R in RStudio

Run the app:

shiny::runApp()


The dashboard will launch automatically in your browser.

🧠 Author & Acknowledgments

Developed as part of the New Jersey Cultural Data Portal Project, integrating spatial, demographic, and cultural data for public access and research use.
