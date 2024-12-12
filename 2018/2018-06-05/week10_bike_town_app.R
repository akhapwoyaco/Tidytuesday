library(shiny)
library(bslib)

# get data
all_files = list.files(path = '.', full.names = T)
#
library(purrr)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
#
# bike_data <- pmap_dfr(
#   .f = fread, 
#   .l = list(all_files),
#   .id = NULL, #index of file 
#   na.strings = "NULL" )
tuesdata <- tidytuesdayR::tt_load('2018-06-05')
bike_data <- tuesdata$week10_biketown
#' 
#' Payment Plan
payment_plans <- c(unique(bike_data$PaymentPlan), "ALL")
#
ui <-  fluidPage(
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "#ED79F9", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  #' TAB 1
  tabsetPanel(
    id = 'tabs',
    tabPanel(
      title = "Introduction",
      fluidPage(
        helpText(
          'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-02, and as such we not 
              hold the right to the data content: ', 
          tags$a("TidyTuesday data 2018-04-02", 
                 href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-06-05"),
          tags$br(),
          tags$code("tuesdata <- tidytuesdayR::tt_load('2018-06-05')"),
          tags$br(),
          tags$code("week10_biketown <- tuesdata$week10_biketown"),
          tags$br()),
        helpText(
          'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
          paste0(
            'The week10_biketown data contains ', dim(bike_data)[1], 
            ' observations, and ', dim(bike_data)[2], ' features on 
                various comic charactesr.')),
        helpText("Worthy conclusion can be made with regard to mosaic plots 
                     on the association between the various comic characters'feature."),
        helpText(
          # include the html for lista and bold text
          tags$h4('Data:'),
          tags$li(
            "Selection is made by the radio buttons on top right box for associations 
            desired based on the publisher."), 
          tags$li("Select Inputs define the features to get 
            associations by"),
          tags$h4("Plots: "),
          tags$li(
            'Add Resource for Mosaic Plot Interpretation and Download Button Functionality')
        )
      )
    ),
    
    tabPanel(
      title = "BIKETOWN",
      fluidPage(
        fluidRow(
          column(3, 
                 sliderInput(
                   inputId = 'date_range', 
                   label = "Start Date", 
                   min = as.Date("2016-07-19", "%Y-%m-%d"), 
                   max = as.Date("2020-08-31", "%Y-%m-%d"),
                   value = c(as.Date("2016-08-31", "%Y-%m-%d"), 
                             as.Date("2017-08-31", "%Y-%m-%d")),
                   timeFormat = "%Y-%m-%d")),
          column(3,
                 selectInput(
                   inputId = 'payment_plans', 
                   label = 'Payment Plan', 
                   choices = payment_plans,
                   selected = "ALL"
                 )),
          column(3,
                 textOutput(
                   outputId = 'total_trips'
                 )),
          column(3,
                 textOutput(
                   outputId = 'average_trip_duration'
                 ))),
        fluidRow(
          column(6,
                 plotOutput(outputId = 'heatmap_plot_tab_2', 
                            height = '600px',width = '90%')),
          column(6, 
                 plotOutput(outputId = 'arealine_plot_tab_2', 
                            height = '600px',width = '90%')))
      )
    )
  )
)

server <- function(input, output, session) {
  data_all <- reactive({
    bike_data |> filter(!is.na(EndDate)) |>
      mutate(
        StartLatitude = coalesce(StartLatitude, Start_Latitude),
        StartLongitude = coalesce(StartLongitude, Start_Longitude),
        EndLatitude = coalesce(EndLatitude, End_Latitude),
        EndLongitude = coalesce(EndLongitude, End_Longitude),
        Distance_Miles = coalesce(Distance_Miles, Distance_Miles_),
        Duration = lubridate::hms(Duration)) |> 
      select(
        -Start_Latitude, -Start_Longitude, -End_Latitude, 
        -End_Longitude, -Distance_Miles_
      ) |>
      unite(
        col = 'EndDateTime',  EndDate:EndTime, sep = ' ', remove = TRUE
      ) |> 
      unite(
        col = 'StartDateTime',  StartDate:StartTime, 
        sep = ' ', remove = TRUE
      ) |>  
      mutate(
        EndDateTime = as.POSIXct(
          x = strptime(EndDateTime, format = "%m/%d/%Y %H:%M", tz = 'GMT')),
        StartDateTime = as.POSIXct(
          x = strptime(StartDateTime, format = "%m/%d/%Y %H:%M", tz = 'GMT'))
      ) |> 
      mutate(
        weekday = lubridate::wday(StartDateTime, label = T),
        hour_day = lubridate::hour(StartDateTime)
      ) |> 
      mutate(
        hour_day = case_when(
          hour_day == 0 ~ '12 am',
          hour_day > 0 & hour_day < 12 ~ paste(hour_day, 'am', sep = ' '),
          hour_day == 12 ~ '12 pm',
          hour_day > 12 ~ paste(hour_day-12, 'pm', sep = ' ')
        ),
        StartDate = lubridate::date(StartDateTime)
      )
  })
  #'
  #' Filter Date
  
  #payment_plans
  
  data_all_2 <- reactive({
    data_ranges <- data_all() |>
       filter(
         StartDateTime < input$date_range[2] & StartDateTime > input$date_range[1])
    
    if (input$payment_plans == "ALL"){
      data_ranges
    } else {
      data_ranges |> 
        filter(PaymentPlan == input$payment_plans)
    }
  })
  #'
  #'
  #' Trips Per Week Heatmap
  #' 
  weekday_hour <- reactive({
    data_all_2() |> 
      select(weekday, hour_day) |> 
      na.omit() |>  group_by(weekday, hour_day) |> 
      summarise(n = n()) |> 
      mutate(
        hour_day = factor(
          hour_day, 
          levels =  c("12 am","1 am","2 am","3 am","4 am","5 am",
                      "6 am","7 am", "8 am", "9 am","10 am","11 am",
                      "12 pm", "1 pm","2 pm","3 pm","4 pm","5 pm",
                      "6 pm","7 pm", "8 pm", "9 pm", "10 pm","11 pm") 
        )
      )
  })
  #' 
  #' 
  output$heatmap_plot_tab_2 <- renderPlot({
    weekday_hour() |>
      ggplot(aes(x = weekday, y = hour_day, fill = n)) + 
      geom_tile(color = 'white', linewidth = 0.1) + 
      geom_text(aes(label = n)) +
      scale_fill_gradient(low = 'yellow', high = 'red') + 
      theme_classic() + 
      theme(
        legend.title = element_blank(),
        legend.position = 'none', axis.title = element_blank()
      )
  })
  #' 
  #' Trips per week Area Plot
  #' 
  trips_week <- reactive({
    data_all_2() |> 
      select(StartDate, StartDateTime) |> 
      mutate(
        #StartDate = lubridate::date(bike_data$StartDateTime),
        week = lubridate::week(StartDateTime)
      ) |> select(StartDate, week) |>
      group_by(
        StartDate, week
      ) |> 
      summarise(
        n = n()
      ) |> na.omit() 
  })
  #'
  #'
  output$arealine_plot_tab_2 <- renderPlot({
    trips_week() |>
      ggplot(aes(x = StartDate, y = n)) + 
      geom_area(color = 'black', fill = 'blue', alpha = 0.5) +
      theme_minimal() + 
      scale_x_date(date_labels = '%b-%y', date_breaks = '2 months') +
      theme(
        legend.title = element_blank(),
        legend.position = 'none', axis.title = element_blank(),
        axis.text.x = element_text(face = 'bold', angle = 90)
      )
  })
  #'
  #' Text Output
  tot_trip <- reactive({dim(data_all_2())[1]})
  aver_dur <- reactive({
    round(
      mean(data_all_2()$Duration, na.rm = TRUE), 2)})
  #'
  output$total_trips <- renderText({
    paste('Total Trips in Time Period: ', tot_trip())
  })
  #'
  output$average_trip_duration <- renderText({
    paste('Average Trip Duration: ', aver_dur(), 'Minutes')
  })
  #' 
  #' 
  #' Longitude Latitude Plot
  lon_lat_data <- reactive({
    data_all_2() |>
      group_by(StartDate, StartHub, StartLatitude, StartLongitude) |> 
      summarise(n = n()) |> print(n = 5)
  })
  #' 
}

shinyApp(ui, server)