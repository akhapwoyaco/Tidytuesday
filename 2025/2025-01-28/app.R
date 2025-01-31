# app/app.R
library(shiny)
library(leaflet)
library(DT)
library(usmap)
library(plotly)
#
source("modules/module_ui.R")
source("modules/module_server.R")

ui <- fluidPage(
  includeCSS("css/main.css"),
  titlePanel(title = "Geographic Data Explorer"),
  geoDataUI("geodata"),
  mainPanel(
    div(class = "average_table_data",
      fluidRow(dataTableOutput("averages_table"))
    )
  )
)

server <- function(input, output, session) {
  geo_data <- geoDataServer("geodata")
  # View(geo_data())
  # 
  # output$map <- renderLeaflet({
  #   req(geo_data())
  #   # geo_data() |> 
  #     # leaflet() |>
  #     # addTiles() |>
  #     # addPolygons(
  #     #   #fillColor = ~colorQuantile("YlOrRd", value)(value),
  #     #   weight = 2,
  #     #   color = "#666",
  #     #   fillOpacity = 0.7#,
  #     #   # popup = ~paste(county, state, "<br>Value:", value)
  #     # )
  # })
  # 
  output$averages_table <- renderDataTable({
    req(geo_data)
    # View(geo_data())
    data = geo_data() |>
      select(county, year, percent_lacking_plumbing) |>
      pivot_wider(
        id_cols = county, names_from = year,
        values_from = percent_lacking_plumbing)
    #
    datatable(
      data = data, 
      options = list(dom = 't'))
  })
}

shinyApp(ui = ui, server = server)