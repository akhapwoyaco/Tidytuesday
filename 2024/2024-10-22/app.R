# Global
library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(readr)

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Map Module UI
mapModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("worldMap"), height = "500px")
  )
}

# Map Module Server
mapModuleServer <- function(id) {
  moduleServer(
    id, function(input, output, session) {
      # Get world map data
      world_map <- reactive({
        # Load world country polygons
        world <- ne_countries(scale = "medium", returnclass = "sf") |>
          select(name_en, geometry, name, economy, income_grp, pop_est)
        return(world)
      })
      
      
      # Create color palette based on population
      income_grp_palette <- reactive({colorFactor(
        palette = 'Paired', domain = world_map()$income_grp
      )})
      #
      income_grp_classification_w <- reactive({
        as.character(unique(
          world_map()$economy)
        )
      })
      
      # pal <- reactive({
      #   colorNumeric(
      #     palette = "viridis",
      #     domain = world_map()$Population,
      #     na.color = "#808080"
      #   )
      # })
      
      # Render the map
      output$worldMap <- renderLeaflet({
        req(world_map())
        # print(world_map())
        leaflet() |>
          # Add base tiles
          addProviderTiles(providers$CartoDB.Positron) |>
          
          # Add country polygons
          addPolygons(
            data = world_map(),
            # Styling
            fillColor = ~income_grp_palette()(income_grp),
            weight = 1,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7,
            # Interaction
            layerId = ~name_en,
            # group = ~economy,
            # Popups
            popup = ~paste0(
              "<strong>", name, "</strong><br>",
              "2019 Est. Population: ", format(pop_est, big.mark = ",", scientific = FALSE)
            ),
            # Highlighting
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          ) |>
          
          # Add legend
          addLegend(
            position = "bottomright",
            pal = income_grp_palette(),
            values = world_map()$income_grp,
            title = "Income Group", opacity = 1,
            labFormat = labelFormat(big.mark = ",")
          ) #|> 
          # addLayersControl(
          #   position = c("bottomleft"),
          #   baseGroups = income_grp_classification_w(),
          #   options = layersControlOptions(collapsed = FALSE)
          # ) |>
          # # https://stackoverflow.com/questions/52413381/add-title-to-layers-control-box-in-leaflet-using-r
          # htmlwidgets::onRender("
          #     function() {
          #         $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center;font-weight:bold;\">Classifications</label>');
          #     }
          # ")
        
        
        
      })
      country_name_id = reactiveVal(NULL)
      # observe the marker click info and print to console when it is changed.
      observeEvent(
        input$worldMap_shape_click, {
          click <- input$worldMap_shape_click
          country_name_id(click[['id']])
        })
      # Return selected country
      return(country_name_id)
    }
  )
}

# Cards Module UI
cardsModuleUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("countryCards"))
}

# Cards Module Server
cardsModuleServer <- function(id, cia_fact_book_data, selected_country) {
  moduleServer(id, function(input, output, session){
    
    country_data_a = reactive({
      req(selected_country())
      print(selected_country())
      # Get data for selected country
      country_data <- cia_fact_book_data |> 
        mutate(
          country = str_replace_all(
            country, 
            c("Curacao" = "Curaçao", "Cabo Verde" = "Cape Verde", 
              "Congo, Democratic Republic of the" = "Democratic Republic of the Congo",
              "Timor-Leste" = "East Timor"
            ))
        ) |>
        dplyr::filter(country == selected_country())
    })
    #
    output$countryCards <- renderUI({
      req(selected_country)
      req(country_data_a())
      
      # Define metrics to display
      metrics <- list(
        "Country" = list(col = "country", suffix = " country"),
        "Area" = list(col = "area", suffix = " sq km"),
        "Birth Rate" = list(col = "birth_rate", suffix = " per 1,000"),
        "Death Rate" = list(col = "death_rate", suffix = " per 1,000"),
        "Infant Mortality" = list(col = "infant_mortality_rate", suffix = " per 1,000"),
        "Internet Users" = list(col = "internet_users", suffix = "users"),
        "Life Expectancy At Birth" = list(col = "life_exp_at_birth", suffix = " years"),
        "Maternal Mortality" = list(col = "maternal_mortality_rate", suffix = " per 100,000"),
        "Net Migration" = list(col = "net_migration_rate", suffix = " per 1,000"),
        "Population" = list(col = "population", suffix = "people"),
        "Population Growth" = list(col = "population_growth_rate", suffix = "%")
      )
      
      # Create cards
      fluidRow(
        lapply(names(metrics), function(metric) {
          column(
            width = 3,
            box(
              width = NULL,
              title = metric,
              paste0(
                format(country_data_a()[[metrics[[metric]]$col]], 
                       big.mark = ",", 
                       scientific = FALSE),
                metrics[[metric]]$suffix
              ),
              status = "primary"
            )
          )
        })
      )
    })
  }
  )
}
# Data Module
dataModuleServer <- function(id) {
  moduleServer(
    id, function(input, output, session) {
      tryCatch({
        print(11111111)
        data <- read_csv("cia_factbook.csv")
        required_cols <- c(
          "country", "area", "birth_rate", "death_rate", "infant_mortality_rate", 
          "internet_users", "life_exp_at_birth", "maternal_mortality_rate", 
          "net_migration_rate", "population", "population_growth_rate"
        )
        #
        missing_cols <- setdiff(required_cols, names(data))
        if (length(missing_cols) > 0) {
          stop("Missing columns: ", paste(missing_cols, collapse = ", "))
        }
        
        return(data)
      }, error = function(e) {
        stop("Error loading data: ", e$message)
      })
    }
  )
}

# UI
ui <- fluidPage(
  titlePanel("CIA Factbook 2014: Data Explorer"),
  
  fluidRow(
    column(12, 
           mapModuleUI("map")
    )
  ),
  
  fluidRow(
    column(12,
           cardsModuleUI("cards")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load data
  cia_fact_book_df <- dataModuleServer("data")
  
  # Initialize map module
  selected_country <- mapModuleServer("map")
  
  # Initialize cards module
  cardsModuleServer("cards",cia_fact_book_data = cia_fact_book_df, 
                    selected_country = selected_country)
}

# Create Shiny app
shinyApp(ui = ui, server = server)