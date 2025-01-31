# app/modules/server.R
geoDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$state)
      file_name_rds = state.abb[match(input$state, state.name)]
      
      path_to_file = list.files(
        path = 'data/', pattern = paste0('^', file_name_rds), 
        full.names = T)
      #
      readRDS(file = path_to_file) |> select(-geoid, -state_abb)
      #
    })
    #
    observe({
      req(input$state)
      req(data())
      counties <- data()$county
      updateSelectInput(session, "county",
                        choices = counties)
    })
    #
    filtered_data <- reactive({
      req(data(), input$state, input$county)
      df <- data()
      if (length(input$county) > 0) {
        df <- df[df$county %in% input$county, ]
      }
    })
    #
    return(filtered_data)
  })
}