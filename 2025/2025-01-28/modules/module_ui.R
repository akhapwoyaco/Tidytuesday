# app/modules/ui.R
# county_state_data = readRDS("data/county_state_data.rds")
county_state_data = readRDS("data/county_state_data.rds")
unique_states = unique(county_state_data$state) |> sort()

geoDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(
        ns("state"), "Select State", choices = unique_states),#state.name),
      selectInput(
        ns("county"), "Select County",
        choices = NULL, multiple = TRUE)
    )
  )
}
