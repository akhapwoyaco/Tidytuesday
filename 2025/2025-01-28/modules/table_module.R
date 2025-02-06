tableUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      dataTableOutput(ns("averages_table"))
    )
  )
}

tableServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      data_pw <- reactive({
        req(data())
        data_pw = data() |>
          select(county, year, percent_lacking_plumbing) |>
          pivot_wider(
            id_cols = county, names_from = year,
            values_from = percent_lacking_plumbing)
      })
      # output
      output$averages_table <- renderDataTable({
        req(data_pw())
        data_pw() |> 
          datatable(
            options = list(
              dom = 'lftip', # length, filter, table, info, pagination
              searching = TRUE,
              pageLength = 115)) |>
          formatStyle(
            columns = 2:3, 
            target = "row",
            color = styleInterval(c(0), c('red', 'green'))
          )
    })
}
  )
  }