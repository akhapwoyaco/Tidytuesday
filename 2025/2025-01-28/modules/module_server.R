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

    # observe( {
    #   updateSelectInput(session, "county", choices = data()$county)
    # })

    observe({
      req(input$state)
      req(data())
      counties <- data()$county
      updateSelectInput(session, "county",
                        choices = counties)
    })
    
    # all_data <- reactive({
    #   us_map(regions = "counties", include = input$state) |>
    #     us_map(regions = "counties", include = "AL") |> 
    #     select(-fips, -full) |> 
    #     mutate(
    #       county = str_replace(county, "County", "") |> trimws(),
    #       geometry = geom
    #     ) |> select(-geom) |>
    #     left_join(
    #       data(), 
    #       by = "county"
    #     ) |> drop_na()
    # })

    return(data)
  })
}