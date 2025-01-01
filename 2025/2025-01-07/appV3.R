library(shiny)
library(readr)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(DT)

# load data
load(file = 'flights_data.RData')
#
time_frequencies = unique(sheet_data_variable_copy$`Time frequency`)
#
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
    body {
      font-family: 'JetBrains Mono', monospace;
      background-color: black;
      color: white;
      height: 100vh;
      line-height: 2.20rem;
    }
    h2 {
        font-size: 3rem;
        font-weight: 700;
        line-height: calc(2* var(--line-height));
        text-transform: uppercase;
        text-align: center;
        left: 0px;
        right: 0px;
        background-color: black;
        display: flex;
        align-items: center;
        justify-content: center
        
    }
    label {
      font-size: 2rem;
      font-weight: 700;
        
        display: flex;
        max-width: 100%;
        margin-bottom: 5px;
        font-weight: 700;
        justify-content: center;
    }
    .row {
      background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
      width: 100%;
      padding-right: 15px;
      padding-left: 15px;
      margin-right: auto;
      margin-left: auto;
      
    }
    #time_frequency, #unit_of_measure, #traffic_and_transport_measurement, #airport_i {
       background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
      font-size: 2rem;
      font-weight: 700;
      
      background-color: black;
      border: 0.5px solid #fff;
      padding: inherit; /*give padding below selected*/
      display: inline-block;
      width: 100%;
      position: relative;
      z-index: 1;
      box-sizing: border-box;
      box-shadow: none;
      border-radius: 0px;
      color: white;

    }
    #DataTables_Table_0 {
      color: white;
      position: relative;
      top: calc(var(--line-height) / 2);
    }
    table, th, td {
      border: 1px solid;
      vertical-align: top;
      color: white;
      border-thickness: 1px;
      text-color: white;
      text-align: left;
    }
    .airport_plot_ggplot2 {
      position: relative;
      overflow: hidden;
      transition: all 0.4s ease-in-out;
      border-radius: 5px;
      box-shadow: 2px 1px 25px 5px rgba(166, 166, 166, 0.2);
      margin: 1rem 0;
      padding: 10px;
      color: #f0f0f0;
      border: 1px solid rgba(17, 72, 126, 0.1);
    }
    #time_frequency > option:nth-child(1)  {
      padding: calc((var(--line-height) / 2)) calc(1ch - var(--border-thickness) / 2) calc((var(--line-height) / 2) -(var(--border-thickness)));
    }
    "))),
  titlePanel("AIRPORT: TIDYTUESDAY 2025-01-07"),
  fluidPage(
    fluidRow(
      #style = "display: flex; flex-wrap:nowrap; gap: 1ch; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
      column(3,
             selectInput(
               inputId = 'time_frequency', 
               label = 'Time Frequency', selectize = F,
               choices = time_frequencies),
             style="margin: 0 auto;display: block;"),
      column(3, 
             uiOutput('unit_of_measure_uio'),
             style="margin: 0 auto;display: block;"
      ),
      column(3,
             uiOutput('traffic_and_transport_measurement_uio'),
             style="margin: 0 auto;display: block;"),
      column(3,
             uiOutput('airport_uio'),
             style="margin: 0 auto;display: block;")
    )),
  br(),
  fluidRow(
    DT::dataTableOutput(outputId = 'airport_data_tab')
  ),
  br(), br(),
  fluidRow(
    plotOutput('airport_plot_ggplot2', height = '600px')
  ),
  br(),
  HTML(
    '<p>The full source code is here: <a href = "https://github.com/akhapwoyaco/" 
      target="_blank">https://github.com/akhapwoyaco</a>
      </a>
      </p>
      <p>
      Data Source: https://ec.europa.eu/eurostat/databrowser/view/avia_par_lu__custom_14829930/bookmark/line?lang=en&bookmarkId=fb7169d9-11d5-47de-9335-fa64107e66ea
      </p>
      <p>Inspiration: <a href = "https://owickstrom.github.io/the-monospace-web/#introduction"
      target="_blank">The Monospace Web/</a>
      </a>
      </p>
      '
  ),
  br(),
  div(
    style = "gap: 1ch; display: flex;
      align-items: center;
      justify-content: center; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
    downloadButton(outputId = "download_image", label = "Download Image")
  ),
  br(),
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
    ))
)

server <- function(input, output, session) {
  ## INPUTS ###################################################
  time_frequencies_header <- reactive({
    sheet_data_variable_copy |>
      filter(`Time frequency` == input$time_frequency) |>
      select(-`Time frequency`)
  })
  #
  output$unit_of_measure_uio <- renderUI({
    req(input$time_frequency)
    #
    unit_measure_unique = time_frequencies_header() |>
      distinct(`Unit of measure`) |> pull()
    #
    selectInput(selectize = F,
                inputId = 'unit_of_measure', label = 'Unit of Measure', 
                choices = unit_measure_unique, multiple = F)
  })
  #
  unit_measure_header <- reactive({
    req(input$unit_of_measure)
    time_frequencies_header() |>
      filter(`Unit of measure` == input$unit_of_measure) |>
      select(-`Unit of measure`)
  })
  #
  output$traffic_and_transport_measurement_uio <- renderUI({
    req(input$unit_of_measure)
    #
    ttm_measure_unique = unit_measure_header() |>
      distinct(`Traffic and transport measurement`) |> 
      pull()
    #
    selectInput(selectize = F,
                inputId = 'traffic_and_transport_measurement',
                label = 'Traffic & Transport', #Measurement 
                choices = ttm_measure_unique, multiple = F)
  })
  #
  ## DATA #######################################################
  #
  series_name <- reactive({
    req(input$traffic_and_transport_measurement)
    unit_measure_header() |>
      filter(
        `Traffic and transport measurement` %in% input$traffic_and_transport_measurement
      ) |> select(series) |> unlist(use.names = F)
  })
  #
  airpots_series_data <- reactive({
    get(series_name())
  })
  #
  airpots_series_val <- reactive({
    #
    unique_airport = airpots_series_data() |> distinct(TIME) |> pull() |> 
      gsub(pattern = "LUXEMBOURG airport - | airport", 
           replacement = "", x = _)
    unique_airport
  })
  # INPUT
  #
  output$airport_uio <- renderUI({
    req(airpots_series_val())
    #
    selectInput(selectize = F,
                inputId = 'airport_i', label = 'Airport', 
                choices = airpots_series_val(), multiple = F)
  })
  #
  series_data <- reactive({
    req(input$airport_i)
    airpots_series_data() |> 
      filter(grepl(x = TIME, pattern = input$airport_i)) |> 
      select(-TIME)
  })
  #
  plot_series_data <- reactive({
    validate(
      need(ncol(series_data()) > 0, message = "NO DATA")
    )
    # print(input$time_frequency )
    # View(series_data())
    series_data_long = series_data() |> 
      pivot_longer(
        cols = everything(), 
        names_to = 'year', 
        values_to = 'passengers') |> 
      mutate(across(!year, .fns = as.integer)) 
    series_data_long |>
      mutate(
        year2 = year, 
        year =  parse_date_time(
          x = year, c( 'yq', 'ym', "Y") ) |> lubridate::date()
      ) |>
      drop_na(passengers)
  })
  #
  # Output data per state
  output$airport_data_tab <- renderDataTable({
    validate(
      need(nrow(series_data()) > 0, message = "NO DATA")
    )
    validate(
      need(
        ncol(series_data()) != rowSums(
          is.na(series_data())
        ), message = "NO OBSERVATIONS DATA")
    )
    #
    series_data() |> 
      mutate(across(everything(), .fns = as.integer)) |>
      pivot_longer(
        cols = everything(), names_sep = "-", 
        names_to = c("year", 'quarter')) |> 
      pivot_wider(
        names_from = "year", values_from = 'value') |>
      drop_na() |>
      datatable(
        options = list(
          dom = 't', scrollX = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")), rownames = F)
  })#
  airport_plot <- reactive({
    #
    validate(
      need(nrow(plot_series_data()) > 0, message = "NO DATA")
    )
    validate(
      need(
        nrow(plot_series_data()) != sum(
          is.na(plot_series_data()$passengers)
        ), message = "NO OBSERVATIONS DATA")
    )
    #
    plot_series_data() |>  
      ggplot(aes(x = year, y = passengers, group = 1)) + 
      geom_line() + geom_point(color = 'blue') +  
      labs(
        y = 'Passengers on board', x = 'Year-Quarter',
        title = paste(
          input$airport_i,
          sep = ' '),
        caption = "https://github.com/akhapwoyaco",
        subtitle = "Air passenger transport routes between partner airports and main airports in Luxembourg") + 
      scale_x_date(date_labels = "%b-%Y", date_breaks = "3 months", expand = c(0,0)) +
      theme_light() + 
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0.5))
  })
  #
  output$airport_plot_ggplot2 <- renderPlot({
    airport_plot()
  })
  #
  # download plot via handler
  output$download_image <- downloadHandler(
    filename = function(){
      # supply state names and paste on filetype
      paste(
        paste0(
          input$time_frequency,
          input$unit_of_measure,
          input$traffic_and_transport_measurement, 
          input$airport_i,
          collapse = '_'),
        '.jpeg', sep = ''
      )
    },
    content = function(file){
      ggsave(
        plot = airport_plot(), filename = file, 
        width = 35, height = 25, units = "cm", dpi = 450
      )
    }
  )
  #
}
shinyApp(ui, server)