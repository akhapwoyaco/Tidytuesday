# app/modules/ui.R
geoDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
    selectInput(ns("state"), "Select State", choices = state.name),
    selectInput(ns("county"), "Select County",
                choices = NULL, multiple = TRUE)
  )
  )
}
