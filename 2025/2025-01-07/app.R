#' 
#' Shiny app to display line plots of each state to note trends
#' in both ggplot and base R
#'
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(bslib)
library(lubridate)
library(DT)
#
avia_par_lu_spreadsheet <- read_excel(
  path = "avia_par_lu_spreadsheet.xlsx", 
  sheet = "Sheet 1", skip = 8, na = c(":", "not available" )) |>
  # exclude columns with entirely missing data
  janitor::remove_empty(which = "cols") |>
  drop_na(TIME) |> slice(-1) |> # remove that first row
  filter(!grepl("Special value", TIME))
# head(avia_par_lu_spreadsheet)
unique_airport = unique(avia_par_lu_spreadsheet$TIME) |> 
  gsub(pattern = "LUXEMBOURG airport - | airport", replacement = "", x = _)
#
ui <- page_sidebar(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
        @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
        body {
          font-family: 'JetBrains Mono', monospace;
          height: 100vh;
          line-height: 2.20rem;
        }
        .navbar-brand {
          margin-right: 0;
        }
        h1 {
            font-size: 3rem;
            font-weight: 700;
            line-height: calc(2* var(--line-height));
            text-transform: uppercase;
            text-align: center;
            left: 0px;
            right: 0px;
            

            display: flex;
            align-items: center;
            justify-content: center}
          .container-fluid {
            width: auto; /*have header at center of page*/
            }
        "))),
  title = 'AIRPORT: TIDYTUESDAY 2025-01-07',
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        title = "NULL",
        list(
          selectInput(inputId = 'airport_i', label = 'Airport', 
                      choices = unique_airport),
          br(),
          # source https://github.com/rstudio/shiny-examples/blob/main/016-knitr-pdf/
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                       inline = TRUE),
          br(),
          downloadButton('downloadReport'),
          br(),
          br(),
          actionButton(inputId='fiver', label="Fiver R Shiny Services", 
                       icon = icon("th"), 
                       onclick ="window.open('https://www.fiverr.com/s/jj5dYam', '_blank')")
        )
      ))),
  accordion(
    open = NULL,
    accordion_panel(
      "TABLE",
    DT::dataTableOutput(outputId = 'airport_data_tab')
    ),
    accordion_panel(
      "Base R Plot",
      plotOutput('airport_plot_base', height = '600px')
    ),
    accordion_panel(
      "GGPLOT2 R Plot",
      plotOutput('airport_plot_ggplot2', height = '600px')
    )), 
  tags$div(
    id='cite', 'Data Source: ', 
    tags$em(
      'https://ec.europa.eu/eurostat/databrowser/view/avia_par_lu__custom_14829930/bookmark/line?lang=en&bookmarkId=fb7169d9-11d5-47de-9335-fa64107e66ea'
      )
  ),
  br(),
  div(
    class = "footer",
    style='height:50px;background:gray54;margin-top:auto;margin-bottom:auto;
                    text-align:center;',
    HTML(
      '<footer class="footer">
              Copyright &copy; 2025 &nbsp; 
              Github Account: <a href="https://github.com/akhapwoyaco"
              target="_blank">akhapwoyaco</a>
              </footer>'
    )
  )
)

server <- function(input, output, session) {
  # Reactive state to capture input state data
  airport_data <- reactive({
    req(input$airport_i)
    # View(unique_airport)
    avia_par_lu_spreadsheet |> 
      filter(grepl(x = TIME, pattern = input$airport_i)) |> 
      select(-TIME)
  }) 
  # long data for ggplot 2
  airport_data_2 <- reactive({
    # pivot so that dates are a column and tuition rates are a column
    airport_data() |> 
      pivot_longer(
        cols = everything(), 
        names_to = 'year', 
        values_to = 'passengers') |> 
      mutate(across(!year, .fns = as.numeric)) |>
      mutate(year = lubridate::yq(year))
  })
  #
  # Output data per state
  output$airport_data_tab <- renderDataTable({
    validate(
      need(nrow(airport_data()) > 0, message = "NO DATA")
    )
    validate(
      need(
        ncol(airport_data()) != rowSums(
          is.na(airport_data())
        ), message = "NO OBSERVATIONS DATA")
    )
    airport_data() |> 
      pivot_longer(
        cols = everything(), names_sep = "-", 
        names_to = c("year", 'quarter')) |> 
      pivot_wider(
        names_from = "year", values_from = 'value') |>
      datatable(
        options = list( dom = 't'), rownames = F
      )
    })#, striped = TRUE, bordered = TRUE)
  # Plot output BASE R
  output$airport_plot_base <- renderPlot({
    #
    # ensure df is non empty, else message
    validate(
      need(nrow(airport_data()) > 0, message = "NO DATA")
    )
    validate(
      need(
        ncol(airport_data()) != rowSums(
          is.na(airport_data())
        ), message = "NO OBSERVATIONS DATA")
    )
    #
    x_val = 1:ncol(airport_data())
    y_val = t(airport_data())
    col_names_data = colnames(airport_data())
    plot(x = x_val, y = y_val,
         ylab = 'Passengers on board', xlab = 'Year-Quarter',
         col = 'blue', type = 'b', pch = 19,
         col.lab = 'black', cex.lab = 1.2, las = 1,
         xaxt = 'n', 
         main = paste(
           input$airport_i, 'Passengers on board',
           sep = ' '))
    axis(1, at = x_val, labels = col_names_data)
    mtext("https://github.com/akhapwoyaco", side = 1, line = 3, padj = 1, adj = 0)
    grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  })
  # Plot output GGPLOT2
  output$airport_plot_ggplot2 <- renderPlot({
    #
    validate(
      need(nrow(airport_data_2()) > 0, message = "NO DATA")
    )
    validate(
      need(
        nrow(airport_data_2()) != sum(
          is.na(airport_data_2()$passengers)
        ), message = "NO OBSERVATIONS DATA")
    )
    #
    airport_data_2() |> 
      ggplot(aes(x = year, y = passengers, group = 1)) + 
      geom_line() + geom_point(color = 'blue') +  
      labs(
        y = 'Passengers on board', x = 'Year-Quarter',
        title = paste(
          input$airport_i,
          sep = ' '),
        caption = "https://github.com/akhapwoyaco",
        subtitle = "Air passenger transport routes between partner airports and main airports in Luxembourg") + 
      scale_x_date(breaks = "%m-%Y", date_breaks = "3 months", expand = c(0,0)) +
      theme_light() + 
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45))
  })
  
}

shinyApp(ui, server)