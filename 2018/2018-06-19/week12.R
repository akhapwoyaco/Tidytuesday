library(shiny)
library(shinydashboard)
library(readr)
library(janitor) #clean names
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(DT)
#
# tuesdata <- tidytuesdayR::tt_load('2018-06-19')
# Google Trends -----------------------------------------------------------
# week12_google_trends <- read_csv(
#   file = "week12_google_trends.csv",
#   show_col_types = FALSE,
#   col_types = cols(Day = col_date(format = "%Y-%m-%d")),
#   skip = 2) |> clean_names()
# # MediaCloud --------------------------------------------------------------
# week12_mediacloud_hurricanes <- read_csv(
#   file = "week12_mediacloud_hurricanes.csv",  
#   show_col_types = FALSE,
#   col_types = cols(Date = col_date(format = "%m/%d/%y")))
# # Mediacloud States -------------------------------------------------------
# week12_mediacloud_states <- read_csv(
#   file = "week12_mediacloud_states.csv", 
#   show_col_types = FALSE,
#   col_types = cols(Date = col_date(format = "%m/%d/%y")))
# # top online news ---------------------------------------------------------
# week12_mediacloud_top_online_news <- read_csv(
#   file = "week12_mediacloud_top_online_news.csv",
#   show_col_types = FALSE,)
# # trump -------------------------------------------------------------------
# week12_mediacloud_trump <- read_csv(
#   file = "week12_mediacloud_trump.csv", 
#   show_col_types = FALSE,
#   col_types = cols(Date = col_date(format = "%Y-%m-%d"))) |> 
#   clean_names()
# # tv Hurricanes -----------------------------------------------------------
# week12_tv_hurricanes <- read_csv(
#   file = "week12_tv_hurricanes.csv", 
#   show_col_types = FALSE,
#   col_types = cols(Date = col_date(format = "%m/%d/%y")))
tuesdata <- tidytuesdayR::tt_load('2018-06-19')
week12_google_trends <- tuesdata$week12_google_trends |> 
  clean_names()
#
week12_mediacloud_hurricanes <- tuesdata$week12_mediacloud_hurricanes |> 
  mutate(
    Date = mdy(Date)#
  )
#
library(readr)
week12_mediacloud_states <- tuesdata$week12_mediacloud_states |> 
  mutate(
    Date = mdy(Date)#
  ) 
#
week12_mediacloud_top_online_news <- tuesdata$week12_mediacloud_top_online_news
#
week12_mediacloud_trump <- tuesdata$week12_mediacloud_trump |>
  mutate(
    Date = ymd(Date)#col_date(format = "%m/%d/%y")
  ) |>
  clean_names()

week12_tv_hurricanes <- tuesdata$week12_tv_hurricanes |> 
  mutate(
    Date = mdy(Date)#col_date(format = "%m/%d/%y")
  )

#'
#' MENU ITEMS
#' 
menu_item_vars <- tibble::tribble(
  ~text, ~tab_name, ~i_con,
  "Introduction", 'data_description', 'gear', 
  "Plots", 'all_plots', 'table'
)
#
tab_naming_function <- function(text = text, tab_name = tab_name, i_con = i_con){
  menuItem(text = text, tabName = tab_name, icon = icon(i_con))
}
#'
ui <- dashboardPage(
  skin = 'blue',
  #Dashboard title
  header = dashboardHeader(title = 'Google Trend', titleWidth = 250),
  #Sidebar layout
  sidebar = dashboardSidebar(
    width = 165,
    sidebarMenu(
      id = "tabs", # Using functions in Shiny
      pmap(menu_item_vars, tab_naming_function)
    ) 
  ),
  
  #Tabs 
  
  body = dashboardBody(
    
    tags$head(
      tags$style(HTML(
        '.main-header .logo {font-weight: bold;} .box-body {font-size: 11px}'
      ))),
    #' 
    tabItems(
      
      # County Population TabItem
      tabItem(
        tabName = 'data_description',
        # Explain the source of data
        box(title = 'Data Source', status = 'success', width = 12,
            height = 150, solidHeader = TRUE, 
            helpText(
              'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-06-19, and as such we not 
              hold the right to the data: ', 
              tags$a("TidyTuesday data 2018-04-30", 
                     href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-06-19"),
              tags$br(),
              tags$code("tuesdata <- tidytuesdayR::tt_load('2018-06-19')"),
              tags$br(),
              tags$code("data <- tuesdata$data")),
            
            helpText(
              'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.')),
        box(title = 'Shiny App Guide', status = 'success', width = 12,
            height = 300, solidHeader = TRUE, 
            helpText(
              # include the html for list and bold text
              tags$h4('Introduction:'),
              tags$li(
                'Source of data is explained, along with period the data covers.'),
              tags$h4("County Population: "),
              tags$li(
                ''),
              tags$h4("Interesting Stuffs: "),
              tags$li(
                'The inclusion of NAs in data tables in place of empty cells.'),
              tags$li(
                'Plotting vector with all NA variables in base R'),
              tags$li(
                'Transpose dataframe and use column 1 as colnames, though 
                differences in the use of %>% and |> (failed to work) arise.'),
              tags$h4("References: "),
              tags$li(
                tags$a("stackoverflow: customizing how datatables displays missing values in shiny"),
                href = 'https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny')
            )
        )
      ),
      #'
      tabItem(
        tabName = 'all_plots',
        fluidPage(
          box(
            title = 'Date Ranges', status = 'primary', width = 12,
            solidHeader = TRUE, height = 140,
            sliderInput(
              inputId = 'date_range', 
              label = NULL,#"Start Date", 
              min = as.Date("2017-08-20", "%Y-%m-%d"), 
              max = as.Date("2017-09-25", "%Y-%m-%d"),
              value = c(as.Date("2017-08-20", "%Y-%m-%d"), 
                        as.Date("2017-09-25", "%Y-%m-%d")),
              timeFormat = "%Y-%m-%d")),
          box(
            title = "week12_google_trends.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            plotOutput(
              outputId = 'out_week12_google_trends_plot', 
              width = '90%', height = 300)),
          box(
            title = "week12_mediacloud_hurricanes.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            plotOutput(
              outputId = 'out_week12_mediacloud_hurricanes_plot', 
              width = '90%', height = 300)),
          box(
            title = "week12_mediacloud_states.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            plotOutput(
              outputId = 'out_week12_mediacloud_states_plot', 
              width = '90%', height = 300)),
          box(
            title = "week12_mediacloud_trump.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            plotOutput(
              outputId = 'out_week12_mediacloud_trump_plot', 
              width = '90%', height = 300)),
          box(
            title = "week12_tv_hurricanes.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            plotOutput(
              outputId = 'out_week12_tv_hurricanes_plot', 
              width = '90%', height = 300)),
          box(
            title = "week12_mediacloud_top_online_news.csv", 
            status = 'primary', width = 6,
            solidHeader = TRUE, height = 400,
            dataTableOutput(
              outputId = 'out_week12_mediacloud_top_online_news_data'
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #'' 
  #' out_week12_google_trends
  #' 
  out_week12_google_trends <- reactive({
    week12_google_trends  |> 
      filter(
        day < input$date_range[2] & day > input$date_range[1]) |>
      pivot_longer(
        cols = !day,
        names_to = 'hurricane',
        values_to = 'values'
      ) |>
      mutate(
        hurricane = str_replace_all(hurricane, c('hurricane_' = "", 
                                                 "_united_states" = ""))
      )
  })
  output$out_week12_google_trends_plot <- renderPlot({
    out_week12_google_trends() |>
      ggplot(aes(x = day, y = values, color = hurricane)) + 
      geom_point() + geom_line() + 
      theme_minimal() + 
      scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
      theme(
        legend.title = element_blank(),
        legend.position = 'top', 
        axis.title = element_blank()
      )
  })
  #'
  #' out_week12_mediacloud_hurricanes
  out_week12_mediacloud_hurricanes <- reactive({
    week12_mediacloud_hurricanes |> 
      filter(
        Date < input$date_range[2] & Date > input$date_range[1]) |>
      pivot_longer(
        cols = !Date,
        names_to = 'hurricane',
        values_to = 'values'
      )
  })
  output$out_week12_mediacloud_hurricanes_plot <- renderPlot({
    out_week12_mediacloud_hurricanes() |>
      ggplot(aes(x = Date, y = values, color = hurricane)) + 
      geom_point() + geom_line() + 
      theme_minimal() + 
      scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
      theme(
        legend.title = element_blank(),
        legend.position = 'top', 
        axis.title = element_blank()
      )
  }, width = 450, height = 300)
  #' 
  #' out_week12_mediacloud_states
  #' 
  out_week12_mediacloud_states <- reactive({
    week12_mediacloud_states |> 
      filter(
        Date < input$date_range[2] & Date > input$date_range[1]) |>
      pivot_longer(
        cols = !Date,
        names_to = 'state',
        values_to = 'values'
      )
  })
  output$out_week12_mediacloud_states_plot <- renderPlot({
    out_week12_mediacloud_states() |>
      ggplot(aes(x = Date, y = values, color = state)) + 
      geom_point() + geom_line() + 
      theme_minimal() + 
      scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
      theme(
        legend.title = element_blank(),
        legend.position = 'top', 
        axis.title = element_blank()
      )
  }, width = 450, height = 300)
  #' 
  #' out_week12_mediacloud_top_online_news
  out_week12_mediacloud_top_online_news <- reactive({
    #'' 
    #'add hyperlinks that on click send you to online news site
    #'
    week12_mediacloud_top_online_news |>
      mutate(
        hyperlink = paste0(
          "<a href='", url, "'>", name, "</a>", sep = '')) |>
      pull(hyperlink) |>
      matrix(nrow = 7, ncol = 7) |>
      as.data.frame(optional = TRUE)
  })
  output$out_week12_mediacloud_top_online_news_data <- renderDT({
    out_week12_mediacloud_top_online_news() |> 
      datatable(
        options = list(
          # got this code from SO, questions/54318475/hide-the-column-names-in-dtdatatable
          headerCallback = JS(
            "function(thead, data, start, end, display){",
            "$(thead).remove();",
            "}"
          ),
          border = '1px solid #ddd',
          dom = 't', #search and pagination hiding
          scrollX = TRUE # scroll across the columns when view reduced
        ),
        escape = FALSE,
        selection = list(mode = 'single', target = 'cell'),
        width = '100%', height = '100%'#table remains in container,
      )
  })
  #' 
  #' out_week12_mediacloud_trump
  out_week12_mediacloud_trump <- reactive({
    week12_mediacloud_trump |> 
      filter(
        date < input$date_range[2] & date > input$date_range[1]) |>
      pivot_longer(
        cols = !date,
        names_to = 'title',
        values_to = 'values'
      ) |>
      mutate(
        title = str_replace_all(title, c('title_' = ""))
      ) 
  })
  output$out_week12_mediacloud_trump_plot <- renderPlot({
    out_week12_mediacloud_trump() |> 
      ggplot(aes(x = date, y = values, color = title)) + 
      geom_point() + geom_line() + 
      theme_minimal() + 
      scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
      theme(
        legend.title = element_blank(),
        legend.position = 'top', 
        axis.title = element_blank()
      )
  }, width = 450, height = 300)
  #'   
  #' out_week12_tv_hurricanes
  out_week12_tv_hurricanes <- reactive({
    week12_tv_hurricanes |> 
      filter(
        Date < input$date_range[2] & Date > input$date_range[1]) |>
      pivot_longer(
        cols = !Date,
        names_to = 'hurricane',
        values_to = 'values'
      )
  })
  output$out_week12_tv_hurricanes_plot <- renderPlot({
    out_week12_tv_hurricanes() |>
      ggplot(aes(x = Date, y = values, color = hurricane)) + 
      geom_point() + geom_line() + 
      theme_minimal() + 
      scale_x_date(date_labels = '%d-%b-%y', date_breaks = '1 week') +
      theme(
        legend.title = element_blank(),
        legend.position = 'top', 
        axis.title = element_blank()
      )
  }, width = 450, height = 300)
  
}

shinyApp(ui, server)