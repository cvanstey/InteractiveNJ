# transit_trends.R
# --------------------------------
# Prepares ACS transportation and age-gender data
# Provides plotting functions for Shiny

library(tidycensus)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(scales)
library(viridis)

# --- Define years and target counties ---
years <- c(2015, 2023)
target_counties <- c(
  "Monmouth County, New Jersey", "Atlantic County, New Jersey", "Ocean County, New Jersey",
  "Cumberland County, New Jersey", "Mercer County, New Jersey", "Essex County, New Jersey",
  "Bergen County, New Jersey", "Salem County, New Jersey", "Burlington County, New Jersey",
  "Camden County, New Jersey", "Gloucester County, New Jersey", "Sussex County, New Jersey",
  "Cape May County, New Jersey", "Middlesex County, New Jersey", "Hunterdon County, New Jersey",
  "Union County, New Jersey", "Passaic County, New Jersey", "Somerset County, New Jersey",
  "Hudson County, New Jersey", "Warren County, New Jersey", "Morris County, New Jersey"
)

# --- Transportation variables ---
transport_vars <- c(
  total = "B08301_001",
  car_alone = "B08301_003",
  carpool = "B08301_004",
  public_transit = "B08301_010",
  bus = "B08301_011",
  subway = "B08301_012",
  train = "B08301_013",
  walked = "B08301_019",
  bicycle = "B08301_018",
  motorcycle = "B08301_017",
  taxi = "B08301_016",
  other = "B08301_020",
  work_home = "B08301_021"
)

# --- Pull ACS transportation data once ---
transport_data <- map_dfr(years, function(y) {
  get_acs(
    geography = "county",
    variables = transport_vars,
    state = "NJ",
    year = y,
    geometry = FALSE
  ) %>%
    mutate(year = y)
})

# --- Clean and filter ---
transport_clean <- transport_data %>%
  filter(NAME %in% target_counties) %>%
  mutate(
    mode = case_when(
      variable == "car_alone" ~ "Drove Alone",
      variable == "carpool" ~ "Carpool",
      variable == "public_transit" ~ "Public Transit",
      variable == "bus" ~ "Bus",
      variable == "subway" ~ "Subway",
      variable == "train" ~ "Train",
      variable == "bicycle" ~ "Bicycle",
      variable == "walked" ~ "Walked",
      variable == "motorcycle" ~ "Motorcycle",
      variable == "taxi" ~ "Taxi",
      variable == "other" ~ "Other",
      variable == "work_home" ~ "Work from Home"
    )
  ) %>%
  filter(!is.na(mode))

# --- Subsets for common plots ---
transit_subset <- transport_clean %>%
  filter(mode %in% c("Public Transit", "Train", "Drove Alone"))

cycle_subset <- transport_clean %>%
  filter(mode == "Motorcycle") %>%
  group_by(NAME) %>%
  mutate(change = estimate[year == 2023] - estimate[year == 2015],
         trend_color = ifelse(change >= 0, "black", "grey70")) %>%
  ungroup()

# --- Plot functions ---

# Public Transit & Train trends
transit_trends_plot <- function() {
  ggplot(transit_subset, aes(x = factor(year), y = estimate, color = mode, group = mode)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~ NAME, scales = "free_y",
               labeller = labeller(NAME = function(x) str_remove(x, " County, New Jersey"))) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_viridis_d() +
    labs(
      title = "Public Transit & Train Commuting Trends by County (NJ)",
      x = "ACS Year",
      y = "Population Estimate",
      color = "Mode"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.x = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.spacing = unit(0.5, "lines")
    )
}

# Motorcycle trends
motorcycle_trends_plot <- function() {
  ggplot(cycle_subset, aes(x = factor(year), y = estimate, group = NAME, color = trend_color)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    facet_wrap(~ NAME, scales = "free_y",
               labeller = labeller(NAME = function(x) str_remove(x, " County, New Jersey"))) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_identity() +
    labs(
      title = "Motorcycle Commuting Trends by County (NJ)",
      x = "ACS Year",
      y = "Population Estimate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.x = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.position = "none",
      panel.spacing = unit(0.5, "lines")
    )
}
