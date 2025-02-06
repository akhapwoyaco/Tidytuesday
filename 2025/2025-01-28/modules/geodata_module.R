# app/modules/ui.R
# county_state_data = readRDS("data/county_state_data.rds")
county_state_data = readRDS("data/county_state_data.rds")
unique_states = unique(county_state_data$state) |> sort()

geoDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(
        ns("state"), "Select State", choices = unique_states),
      selectInput(
        ns("county"), "Select County",
        choices = NULL, multiple = TRUE)
    )
  )
}

# app/modules/server.R
geoDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    state_data <- reactive({
      req(input$state)
      file_name_rds = input$state
      
      path_to_file = list.files(
        path = 'data/', pattern = paste0('^', file_name_rds), 
        full.names = T)
      #
      readRDS(file = path_to_file) |> select(-geoid)
      #
    })
    #
    observe({
      req(input$state)
      req(state_data())
      counties <- state_data() |> 
        pull(county) |> unique()
      #
      updateSelectInput(session, "county",
                        choices = counties)
    })
    #
    filtered_data <- reactive({
      req(state_data(), input$state, input$county)
      df <- state_data()
      
      if (length(input$county) > 0) {
        df <- df[df$county %in% input$county, ]
      }
    })
    #
    return(filtered_data)
  })
}
