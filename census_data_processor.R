# --- NJ Transit & GLAM Accessibility Dashboard ---
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(RColorBrewer)
library(ggplot2)
library(purrr)
library(stringr)

#setwd("")

# --- Load pre-processed data ---
load("data/NJ_data_clean.RData")
ls()
source("census_data_module.R")
source("transit_trends.R")

# --- Census Data ---
county_income <- get_county_income(year = 2023) %>%
  mutate(NAME = gsub(" County, New Jersey", "", NAME))

nj_counties_pop <- nj_counties_pop %>%
  left_join(county_income, by = "NAME")

age_gender_list <- get_age_gender_data(years = c(2015, 2023))
age_gender_agg <- age_gender_list$aggregated
age_levels <- age_gender_list$age_levels

# --- Institutions Combined ---
institutions <- bind_rows(
  # ... rest of your code
)

# --- Institutions Combined ---
institutions <- bind_rows(
  # GLAM data - has Type column for categories
  glam_sf %>%
    filter(!is.na(name) & trimws(name) != "") %>%
    mutate(
      category = Type,
      source = "GLAM",
      address = ifelse(!is.na(full_address) & trimws(full_address) != "", 
                       full_address, 
                       paste(Address1, City, State, ZIP, sep = ", ")),
      url = Website
    ) %>%
    select(name, latitude, longitude, category, source, address, url),
  
  # Monmouth locations - needs coordinate transformation from UTM to lat/long
  monmouth_locations %>%
    mutate(
      address = NA_character_,
      url = NA_character_
    ),
  
  # Higher education
  higher_ed %>%
    mutate(
      category = "Higher Education",
      source = "Higher Education",
      address = paste(`HD2024.Street address or post office box`, 
                      `HD2024.City location of institution`,
                      `HD2024.State abbreviation`,
                      `HD2024.ZIP code`, sep = ", "),
      url = NA_character_
    ) %>%
    select(name, latitude, longitude, category, source, address, url),
  
  # Libraries - add address and url columns if they don't exist
  libraries_clean %>%
    mutate(
      address = if("address" %in% names(libraries_clean)) address else NA_character_,
      url = if("url" %in% names(libraries_clean)) url else NA_character_
    )
)

# --- Institution Colors ---
all_categories <- unique(institutions$category)
institution_colors <- c(
  "Historical Society" = "#E41A1C",
  "Museum" = "#377EB8",
  "Bookstore" = "#4DAF4A",
  "Cultural Site" = "#984EA3",
  "Higher Education" = "#FF7F00",
  "Libraries" = "#FFFF33",
  "Historic Site" = "#A65628",
  "Marine" = "#F781BF",
  "Organization" = "#999999",
  "Lighthouse" = "#66C2A5",
  "Education" = "#FC8D62",
  "Wildlife" = "#8DA0CB",
  "Archives" = "#E78AC3",
  "Gift shop" = "#A6D854"
)

remaining_cats <- setdiff(all_categories, names(institution_colors))
if (length(remaining_cats) > 0) {
  extra_colors <- colorRampPalette(brewer.pal(8, "Pastel1"))(length(remaining_cats))
  institution_colors <- c(institution_colors, setNames(extra_colors, remaining_cats))
}

# --- Utility Functions ---
`%||%` <- function(a, b) if (!is.null(a)) a else b
clean_county_name <- function(name) gsub(" County, New Jersey", "", name)


# --- UI ---
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "NJ Transit & Cultural Access"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactive Map", tabName = "map", icon = icon("map")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Census Plots", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Transit Trends", tabName = "transit", icon = icon("train"))
    ),
    hr(),
    
    # County filter
    selectInput(
      "county_filter",
      "Select County:",
      choices = c(
        "All Counties" = "all",
        setNames(
          sort(nj_counties_pop$NAME),
          sort(clean_county_name(nj_counties_pop$NAME))
        )
      ),
      selected = "all"
    ),
    actionButton("clear_county", "Deselect County"),
    
    # Institutions filter
    checkboxGroupInput(
      "institution_filter",
      "Institution Types:",
      choices = names(institution_colors),
      selected = names(institution_colors)
    ),
    actionButton("clear_institutions", "Deselect All Institutions"),
    
    # Transit filter
    checkboxGroupInput(
      "transit_filter",
      "Transit Types:",
      choices = c("Rail", "Bus"),
      selected = c("Rail", "Bus")
    ),
    actionButton("clear_transit", "Deselect All Transit")
  ),
  
  # --- Dashboard Body ---
  dashboardBody(
    tabItems(
      tabItem(tabName = "transit",
              h2("Public Transit & Train"),
              plotOutput("transit_plot", height = "600px"),
              h2("Motorcycle Trends"),
              plotOutput("motorcycle_plot", height = "600px")
      ),
      
      tabItem(tabName = "map",
              fluidRow(
                valueBoxOutput("selected_county_pop", width = 4),
                valueBoxOutput("total_institutions", width = 4),
                valueBoxOutput ("inst_per_1000", width = 4)
              ),
              fluidRow(
                valueBoxOutput("transit_count", width = 4),
                valueBoxOutput("libraries_count", width = 4),
                valueBoxOutput("historical_society_count", width = 4)                ,
              ),
              leafletOutput("main_map", height = 600)
      ),
      
      tabItem(tabName = "table",
              DTOutput("institutions_table")
      ),
      
      tabItem(tabName = "plots",
              fluidRow(
                box(plotOutput("income_plot"), width = 6, title = "Median Household Income by County"),
                box(plotOutput("population_plot"), width = 6, title = "Population by County")
              ),
              fluidRow(
                box(plotOutput("age_gender_bar"), width = 12, title = "Population by Age and Gender"),
                box(plotOutput("age_gender_line"), width = 12, title = "Age/Gender Trends Over Time")
              )
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  pal <- colorNumeric("Greens", domain = nj_counties_pop$Population)
  
  # --- Reactive Filters ---
  filtered_institutions <- reactive({
    inst <- institutions %>% filter(category %in% input$institution_filter)
    if (!is.null(input$county_filter) && input$county_filter != "all" & nrow(inst) > 0) {
      county_geom <- nj_counties_pop %>% filter(NAME == input$county_filter)
      if (nrow(county_geom) > 0) {
        inst_sf <- inst %>%
          filter(!is.na(latitude) & !is.na(longitude)) %>%  # remove rows with missing coords
          st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
        inst_sf <- st_transform(inst_sf, st_crs(county_geom))
        
        # wrap st_filter in tryCatch
        inst_sf <- tryCatch(
          st_filter(inst_sf, county_geom, .predicate = st_within),
          error = function(e) tibble()  # return empty tibble on error
        )
        
        inst <- if(nrow(inst_sf) > 0) inst_sf %>% st_drop_geometry() else tibble()
      }
    }
    inst
  })
  
  filtered_transit <- reactive({
    if(exists("all_stops_sf")) {
      all_stops_sf %>%
        filter(type %in% input$transit_filter) %>%
        mutate(coords = st_coordinates(geometry)) %>%
        mutate(lng = coords[,1], lat = coords[,2])
    } else tibble()
  })
  
  # --- Base Map ---
  output$main_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = nj_counties_pop,
        fillColor = ~pal(Population),
        fillOpacity = 0.5,
        color = "white",
        weight = 2,
        popup = ~paste0(
          "<b>", NAME, "</b><br>",
          "Population: ", formatC(Population, format="d", big.mark=","), "<br>",
          "Median Household Income: $", formatC(median_income, format="d", big.mark=",")
        )
      ) %>%
      setView(lng = -74.5, lat = 40, zoom = 8)
  })
  
  # --- Map Observer ---
  observe({
    inst <- filtered_institutions()
    transit <- filtered_transit()
    
    leafletProxy("main_map") %>% clearMarkers() %>% clearShapes()
    
    # County polygons
    leafletProxy("main_map") %>%
      addPolygons(
        data = nj_counties_pop,
        fillColor = ~pal(Population),
        fillOpacity = 0.5,
        color = "white",
        weight = 2,
        popup = ~paste0(
          "<b>", NAME, "</b><br>",
          "Population: ", formatC(Population, format = "d", big.mark = ","), "<br>",
          "Median Household Income: $", formatC(median_income, format = "d", big.mark = ",")
        )
      )
    
    # Highlight selected county
    if (!is.null(input$county_filter) && input$county_filter != "all") {
      county_geom <- nj_counties_pop %>% filter(NAME == input$county_filter)
      if (nrow(county_geom) > 0) {
        leafletProxy("main_map") %>%
          addPolygons(data = county_geom, fill = FALSE, color = "blue", weight = 3)
      }
    }
    
    # Transit markers
    if (nrow(transit) > 0) {
      leafletProxy("main_map") %>%
        addCircleMarkers(
          data = transit,
          lng = ~lng, lat = ~lat,
          color = ~ifelse(type == "Rail", "green", "gold"),
          radius = ~ifelse(type == "Rail", 3, 2),
          fillOpacity = 0.9,
          stroke = FALSE,
          popup = ~paste0("<b>", stop_name, "</b><br>Type: ", type)
        )
    }
    
    # Institution markers
    if (nrow(inst) > 0) {
      for (cat in unique(inst$category)) {
        cat_data <- inst %>% filter(category == cat)
        popup_html <- paste0(
          "<b>", cat_data$name, "</b><br>",
          "Type: ", cat_data$category, "<br>",
          ifelse(!is.na(cat_data$address) & trimws(cat_data$address) != "", 
                 paste0("Address: ", cat_data$address, "<br>"), ""),
          ifelse(!is.na(cat_data$url) & trimws(cat_data$url) != "", 
                 paste0('<a href="', cat_data$url, '" target="_blank">Website</a><br>'), "")
        )
        leafletProxy("main_map") %>%
          addCircleMarkers(
            data = cat_data,
            lng = ~longitude, lat = ~latitude,
            radius = 6,
            color = "black",
            fillColor = institution_colors[[cat]] %||% "gray",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 1,
            popup = popup_html
          )
      }
    }
  })
  
  # --- Deselect Buttons ---
  observeEvent(input$clear_county, {
    updateSelectInput(session, "county_filter", selected = "all")
  })
  
  observeEvent(input$clear_institutions, {
    updateCheckboxGroupInput(session, "institution_filter", selected = character(0))
  })
  
  observeEvent(input$clear_transit, {
    updateCheckboxGroupInput(session, "transit_filter", selected = character(0))
  })
  
  # --- Value Boxes ---
  output$selected_county_pop <- renderValueBox({
    if (input$county_filter == "all") {
      pop <- sum(nj_counties_pop$Population, na.rm = TRUE)
      label <- "Total NJ Population"
    } else {
      county_data <- nj_counties_pop %>%
        filter(NAME == input$county_filter)
      pop <- ifelse(nrow(county_data) > 0, county_data$Population[1], 0)
      label <- paste(input$county_filter, "Pop.")
    }
    
    valueBox(
      value = scales::comma(pop),
      subtitle = label,
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$transit_count <- renderValueBox({
    valueBox(
      nrow(filtered_transit()),
      "Transit Stops",
      icon = icon("train"),
      color = "green"
    )
  })
  
  output$libraries_count <- renderValueBox({
    valueBox(
      nrow(filtered_institutions() %>% filter(category == "Library")),
      "Libraries",
      icon = icon("book"),
      color = "yellow"
    )
  })
  
  output$historical_society_count <- renderValueBox({
    valueBox(
      nrow(filtered_institutions() %>% filter(category == "Historical Society")),
      "Historical Societies",
      icon = icon("landmark"),
      color = "red"
    )
  })
  
  output$total_institutions <- renderValueBox({
    valueBox(
      nrow(filtered_institutions()),
      "Total Institutions",
      icon = icon("university"),
      color = "blue"
    )
  })
  
  output$inst_per_1000 <- renderValueBox({
    if (input$county_filter == "all") {
      pop <- sum(nj_counties_pop$Population, na.rm = TRUE)
    } else {
      county_data <- nj_counties_pop %>%
        filter(NAME == input$county_filter)
      pop <- ifelse(nrow(county_data) > 0, county_data$Population[1], 1)
    }
    
    ratio <- round((nrow(filtered_institutions()) / pop) * 1000, 2)
    
    valueBox(
      ratio,
      "Institutions per 1,000",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  # --- Plots ---
  
  output$income_plot <- renderPlot({
    nj_counties_pop %>%
      st_drop_geometry() %>%
      ggplot(aes(x = reorder(NAME, median_income), y = median_income, fill = median_income)) +
      geom_col() +
      coord_flip(expand = FALSE) +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Median Income") +
      scale_y_continuous(
        labels = scales::dollar_format(prefix = "$", big.mark = ","),
        expand = expansion(mult = c(0, 0.05))  # adds space at the right
      ) +
      labs(
        x = "",
        y = "",
        title = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 0, hjust = 0.5),  # keeps x-axis labels horizontal
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
  })
  
  
  
  output$population_plot <- renderPlot({
    nj_counties_pop %>%
      st_drop_geometry() %>%
      ggplot(aes(x = reorder(NAME, Population), y = Population, fill = Population)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "navy", name = "Population") +
      labs(
        x = "",
        y = "",
        title = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
  })
  
  output$age_gender_bar <- renderPlot({
    ggplot(age_gender_agg, aes(x = age, y = estimate, fill = gender)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("Male" = "#377EB8", "Female" = "#E41A1C")) +
      labs(
        title = "By NJ Counties",
        x = "Age Group",
        y = "",
        fill = "Gender"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  output$age_gender_line <- renderPlot({
    # --- Compute top 3 population increases from previous year to 2023 ---
    age_gender_changes <- age_gender_agg %>%
      group_by(age, gender) %>%
      arrange(year) %>%
      mutate(pct_change = (estimate - lag(estimate)) / lag(estimate) * 100) %>%
      ungroup() %>%
      filter(year == 2023 & !is.na(pct_change)) %>%
      arrange(desc(pct_change)) %>%
      slice_head(n = 5)
    
    ggplot(age_gender_agg, aes(x = year, y = estimate, color = gender, group = gender)) +
      geom_line(size = 1) +
      geom_point(size = 2.5, alpha = 0.9) +  # emphasized points
      facet_wrap(~age, scales = "free_y", ncol = 6) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = c("Male" = "#377EB8", "Female" = "#E41A1C")) +
      labs(
        title = "By NJ Counties",
        subtitle = "Top 3 groups with highest % increase to 2023 labeled",
        x = "Census Year",
        y = "",
        color = "Gender"
      ) +
      # --- Gridline + theme cleanup ---
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major = element_line(color = "gray85", size = 0.3),
        panel.grid.minor = element_line(color = "gray90", size = 0.2),
        strip.text = element_text(face = "bold", size = 9),
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11, color = "gray40")
      ) +
      # --- Label top 3 increases ---
      geom_text(
        data = age_gender_changes,
        aes(
          x = year,
          y = estimate,
          label = paste0("+", round(pct_change, 1), "%"),
          color = gender
        ),
        size = 3.2,
        vjust = -0.8,
        fontface = "bold",
        show.legend = FALSE
      )
  })
  
  
  
  output$transit_plot <- renderPlot({ transit_trends_plot() })
  output$motorcycle_plot <- renderPlot({ motorcycle_trends_plot() })
  
  # --- Data Table ---
  output$institutions_table <- renderDT({
    filtered_institutions() %>%
      select(Name = name, Category = category, Latitude = latitude, Longitude = longitude) %>%
      datatable(options = list(pageLength = 25, scrollX = TRUE), filter = "top")
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)


