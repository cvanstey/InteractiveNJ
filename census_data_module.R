# census_data_module.R

library(tidycensus)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)

# --- NJ Counties of Interest ---
nj_target_counties <- c(
  "Monmouth County, New Jersey", "Atlantic County, New Jersey",
  "Ocean County, New Jersey", "Cumberland County, New Jersey",
  "Mercer County, New Jersey", "Essex County, New Jersey",
  "Bergen County, New Jersey", "Salem County, New Jersey",
  "Burlington County, New Jersey", "Camden County, New Jersey",
  "Gloucester County, New Jersey", "Sussex County, New Jersey",
  "Cape May County, New Jersey", "Middlesex County, New Jersey",
  "Hunterdon County, New Jersey", "Union County, New Jersey",
  "Passaic County, New Jersey", "Somerset County, New Jersey",
  "Hudson County, New Jersey", "Warren County, New Jersey",
  "Morris County, New Jersey"
)

# --- Get Age/Gender Data ---
get_age_gender_data <- function(years = c(2015, 2023), target_counties = nj_target_counties) {
  
  # --- Age Variables ---
  male_vars <- paste0("B01001_", sprintf("%03d", 3:25))
  female_vars <- paste0("B01001_", sprintf("%03d", 27:49))
  age_vars <- c(male_vars, female_vars)
  
  age_levels <- c(
    "Under 5", "5 to 9", "10 to 14", "15 to 17", "18 to 19",
    "20", "21", "22 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54",
    "55 to 59", "60 to 61", "62 to 64", "65 to 66",
    "67 to 69", "70 to 74", "75 to 79", "80 to 84", "85+"
  )
  
  # --- Map variable to age/gender ---
  map_age_gender <- function(var) {
    gender <- ifelse(str_detect(var, "^B01001_0(0[3-9]|1[0-9]|2[0-5])"), "Male", "Female")
    age <- case_when(
      var %in% c("B01001_003", "B01001_027") ~ "Under 5",
      var %in% c("B01001_004", "B01001_028") ~ "5 to 9",
      var %in% c("B01001_005", "B01001_029") ~ "10 to 14",
      var %in% c("B01001_006", "B01001_030") ~ "15 to 17",
      var %in% c("B01001_007", "B01001_031") ~ "18 to 19",
      var %in% c("B01001_008", "B01001_032") ~ "20",
      var %in% c("B01001_009", "B01001_033") ~ "21",
      var %in% c("B01001_010", "B01001_034") ~ "22 to 24",
      var %in% c("B01001_011", "B01001_035") ~ "25 to 29",
      var %in% c("B01001_012", "B01001_036") ~ "30 to 34",
      var %in% c("B01001_013", "B01001_037") ~ "35 to 39",
      var %in% c("B01001_014", "B01001_038") ~ "40 to 44",
      var %in% c("B01001_015", "B01001_039") ~ "45 to 49",
      var %in% c("B01001_016", "B01001_040") ~ "50 to 54",
      var %in% c("B01001_017", "B01001_041") ~ "55 to 59",
      var %in% c("B01001_018", "B01001_042") ~ "60 to 61",
      var %in% c("B01001_019", "B01001_043") ~ "62 to 64",
      var %in% c("B01001_020", "B01001_044") ~ "65 to 66",
      var %in% c("B01001_021", "B01001_045") ~ "67 to 69",
      var %in% c("B01001_022", "B01001_046") ~ "70 to 74",
      var %in% c("B01001_023", "B01001_047") ~ "75 to 79",
      var %in% c("B01001_024", "B01001_048") ~ "80 to 84",
      var %in% c("B01001_025", "B01001_049") ~ "85+",
      TRUE ~ NA_character_
    )
    list(gender = gender, age = age)
  }
  
  # --- Pull ACS Data ---
  age_gender_data <- map_dfr(years, function(y) {
    get_acs(
      geography = "county",
      variables = age_vars,
      state = "NJ",
      year = y,
      geometry = FALSE
    ) %>%
      filter(NAME %in% target_counties) %>%
      mutate(
        year = y,
        map = purrr::map(variable, map_age_gender),
        gender = purrr::map_chr(map, "gender"),
        age = purrr::map_chr(map, "age")
      ) %>%
      select(NAME, year, gender, age, estimate)
  })
  
  # --- Aggregate ---
  agg_data <- age_gender_data %>%
    group_by(year, gender, age) %>%
    summarise(estimate = sum(estimate), .groups = "drop") %>%
    mutate(age = factor(age, levels = age_levels, ordered = TRUE))
  
  list(raw = age_gender_data, aggregated = agg_data, age_levels = age_levels)
}

get_county_income <- function(year = 2023) {
  get_acs(
    geography = "county",
    state = "NJ",
    variables = "B19013_001",   # median household income
    year = year,
    survey = "acs5"
  ) %>%
    rename(median_income = estimate)   # rename here
}
