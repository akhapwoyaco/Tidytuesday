# app/app.R
library(tidyverse)
library(shiny)
library(leaflet)
library(DT)
#
source("modules/geodata_module.R")
source("modules/table_module.R")
#
ui <- fluidPage(
  includeCSS("css/main.css"),
  titlePanel(title = "Geographic Data Explorer"),
  geoDataUI("the_geodata_ui"),
  tableUI("c_averages_table")
)
#
server <- function(input, output, session) {
  geo_data <- geoDataServer("the_geodata_ui")
  tableServer("c_averages_table", data = geo_data)
}

shinyApp(ui = ui, server = server)