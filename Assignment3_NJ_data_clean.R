# --- load_data.R ---

# --- Libraries ---
library(sf)
library(tidyverse)
library(readr)
library(tidytransit)
library(tidycensus)
library(leaflet)

#Hi Professor Dunn. I'm not sure how sharing works. I am hoping you will be able to save this file as the "collector and cleaner" 
#Then you will set the working directory and I hope you have an API key. I saved one in the Renviron but just in case, the README has 
#instructions on how to get a key. 
#I think you will need to edit the paths but honestly, I've never shared a file with such complexity other than GitHub or a network folder. 
#I'm really sorry for all the files. I wasn't thinking and I went overboard.
#I used a lot of online resources including chatGPT, claudeAI, CoPilot, Stack, Github, other websites. 
#Among them was this https://data-monmouthnj.hub.arcgis.com/datasets/MonmouthNJ::monmouth-county-historic-sites-inventory-public-view/explore
#Its ARC GIS but helpful. NJ Department of Community Affairs OpenData had a really nice one also on GIS https://njdca.maps.arcgis.com/apps/webappviewer/index.html?id=96ec274c50a34890b23263f101e4ad9b
#It has a public transit component as well. 
#My addition of historic societies might help the NJDCA plot- or it would be a good test because I go into stops rather than just terminals. I grabbed the attribute tables
#for their Arts and Entertainment categories. 
#I had a little trouble with the monmouth assets. The .csv had the wrong lats and longs. I had to pull in the shapefile. 


# --- Working Directory ---
##setwd("")

# --- Census API Key ---
# tidycensus::census_api_key("insert_your_key_here", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# --- Population Data ---
nj_pop <- get_acs(
  geography = "county",
  state = "NJ",
  variables = "B01003_001",
  year = 2021,
  survey = "acs5"
) %>%
  select(NAME, estimate) %>%
  rename(County = NAME, Population = estimate)

nj_pop$County <- gsub(" County, New Jersey", "", nj_pop$County)
nj_pop$County <- gsub(" County", "", nj_pop$County)

# --- Local Spatial + CSV Data ---
# In load_data.R, replace the monmouth_locations section:

monmouth_geo <- st_read("data/Monmouth_County_Historic_Sites_Inventory_(Public_View).geojson")

# Transform to WGS84 and extract proper lat/long
monmouth_locations <- monmouth_geo %>%
  st_transform(4326) %>%  # Ensure WGS84
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2],
    name = Name,
    category  = "Historic Site",
    source    = "Monmouth County"
  ) %>%
  select(name, latitude, longitude, category, source) %>%
  st_drop_geometry()

glam_sf <- read_csv(
  "data/geocoded_spreadsheet.csv",
  show_col_types = FALSE
) %>%
  rename(name = MemberOrganizations, latitude = Latitude, longitude = Longitude)

higher_ed_1 <- read_csv(
  "data/nj_higher_ed_geocoded.csv", 
  show_col_types = FALSE
)

higher_ed <- higher_ed_1 %>%
  rename(
    name = `institution name`,
    latitude = `HD2024.Latitude location of institution`, 
    longitude = `HD2024.Longitude location of institution`
  )
  

# --- Libraries ---
libraries <- st_read(
  "https://services.arcgis.com/Aur8tCo478N3VovT/arcgis/rest/services/Libraries_2023/FeatureServer/11/query?where=1=1&outFields=*&outSR=4326&f=geojson"
)

libraries_clean <- libraries %>%
  mutate(
    longitude = st_coordinates(geometry)[,1],
    latitude  = st_coordinates(geometry)[,2],
    category  = "Library",
    source    = "Library Data",
    name = Library  # Add this line to create the name column
  ) %>%
  select(name, latitude, longitude, category, source) %>%
  st_drop_geometry()



# --- Shapefiles / GeoJSONs ---
counties <- st_read("data/NJCounties.shp")
nj_counties <- counties

rmha_map <- st_read("data/RMHA-map-2.geojson")

# --- Transit Routes (GTFS) ---
gtfs_data <- read_gtfs("https://www.njtransit.com/rail_data.zip")
gtfs_bus <- read_gtfs("https://www.njtransit.com/bus_data.zip")

gtfs_sf <- gtfs_as_sf(gtfs_data)
gtfs_bus_sf <- gtfs_as_sf(gtfs_bus)

stops_sf <- gtfs_sf$stops %>% mutate(type = "Rail")
bus_stops_sf <- gtfs_bus_sf$stops %>% mutate(type = "Bus")

all_stops_sf <- rbind(stops_sf, bus_stops_sf)

# --- Flatten Coordinates ---
stops_df <- stops_sf %>%
  st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

bus_stops_df <- bus_stops_sf %>%
  st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

# --- Harmonize CRS ---
counties <- st_transform(counties, 4326)
rmha_map <- st_transform(rmha_map, 4326)
all_stops_sf <- st_transform(all_stops_sf, 4326)

# --- Join Population ---
nj_counties_pop <- left_join(nj_counties, nj_pop, by = c("NAME" = "County"))

# --- Sanity Check ---
datasets <- list(counties, rmha_map, all_stops_sf)
lapply(datasets, function(x) st_crs(x)$epsg)

# --- Save Clean Workspace ---
save(
  nj_counties_pop, nj_counties, counties, rmha_map,
  monmouth_locations, glam_sf, higher_ed,
  libraries_clean,
  all_stops_sf, stops_df, bus_stops_df,
  file = "data/NJ_data_clean.RData"
)




