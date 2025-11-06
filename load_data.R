# --- load_data.R ---

# --- Libraries ---
library(sf)
library(tidyverse)
library(readr)
library(tidytransit)
library(tidycensus)

# --- Working Directory ---
setwd("C:/Users/crook/Documents")

# --- Load Census API Key ---
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

# --- Local CSV files ---
monmouth_locations <- read_csv("DSSA Program/Monmouth_County_Historic_Sites_Inventory_PV.csv")
glam <- read_csv("DSSA Program/geocoded_spreadsheet.csv")
higher_ed <- read_csv("DSSA Program/New_Jersey_Culture_Portal/nj_higher_ed_geocoded.csv")

# --- Shapefiles and GeoJSONs ---
counties <- st_read("DSSA Program/New_Jersey_Culture_Portal/NJCounties.shp")
nj_counties <- counties
rmha_map <- st_read("DSSA Program/New_Jersey_Culture_Portal/RMHA-map-2.geojson")

# --- Online GeoJSON (Libraries layer from ArcGIS REST API) ---
libraries <- st_read("https://services.arcgis.com/Aur8tCo478N3VovT/arcgis/rest/services/Libraries_2023/FeatureServer/11/query?where=1=1&outFields=*&outSR=4326&f=geojson")

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
  mutate(lng = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

bus_stops_df <- bus_stops_sf %>%
  st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# --- Harmonize CRS ---
counties <- st_transform(counties, 4326)
rmha_map <- st_transform(rmha_map, 4326)
libraries <- st_transform(libraries, 4326)
all_stops_sf <- st_transform(all_stops_sf, 4326)

# --- Join Population ---
nj_counties_pop <- left_join(nj_counties, nj_pop, by = c("NAME" = "County"))

# --- Sanity Check ---
datasets <- list(counties, rmha_map, libraries, all_stops_sf)
lapply(datasets, function(x) st_crs(x)$epsg)

save(
  nj_counties_pop, counties, rmha_map, libraries,
  monmouth_locations, glam, higher_ed,
  all_stops_sf, stops_df, bus_stops_df,
  file = "DSSA Program/NJ_data_clean.RData"
)


