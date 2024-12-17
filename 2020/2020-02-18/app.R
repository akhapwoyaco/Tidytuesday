library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
#library(plotly)
library(DT)
#
tuesdata <- tidytuesdayR::tt_load(2020, week = 8)
food_consumption <- tuesdata$food_consumption

# food_consumption <- read_csv(
#   file = "food_consumption.csv",
#   show_col_types = FALSE)
food_category = unique(food_consumption$food_category)
my_vars = names(food_consumption[sapply(food_consumption, is.numeric)])
#
world_map_data <- map_data('world') |>
  filter(region != "Antarctica") |>
  fortify()
#
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
  skin = 'green',
  #Dashboard title
  header = dashboardHeader(title = 'Food Consumption and CO2 Emissions', titleWidth = 250),
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
              TidyTuesday data 2018-04-30, and as such we not 
              hold the right to the data: ', 
              tags$a("TidyTuesday data 2018-06-26", 
                     href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-01-18"),
              tags$br(),
              tags$code("tuesdata <- tidytuesdayR::tt_load('2020-01-18')')"),
              tags$br(),
              tags$code("food_consumption <- tuesdata$food_consumption")),
            
            helpText(
              'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
              paste0(
                'The data contains ', dim(food_consumption)[1], 
                'observations, on states within across the globe, with ', dim(food_consumption)[2], 
                ' features')))
      ),
      #'
      tabItem(
        tabName = 'all_plots',
        fluidPage(
          box(
            title = NULL, #"Variable", 
            status = 'success', width = 12,
            solidHeader = TRUE, #height = 50,
            column(width = 6, align = 'center',
                   radioButtons(
                     inputId = 'food_category', label = "Food",#'Select Variable: ', 
                     choices = food_category,# width = '50%',
                     inline = TRUE)),
            column(width = 6, align = 'center',
                   radioButtons(
                     inputId = 'variable_all', label = "",#'Select Variable: ', 
                     choices = my_vars,# width = '50%',
                     inline = TRUE))),
          br(),
          box(
            title = NULL,#"World Plot", 
            status = 'success', width = 12,
            solidHeader = TRUE, height = 480,
            plotOutput(
              outputId = 'out_world_plot',
              hover = hoverOpts(id = 'plot_hover', delay = 0.00005),
              width = '90%', height = 400)),
          box(
            title = NULL,#"Country Data", 
            status = 'success', width = 12,
            solidHeader = TRUE, #height = 300,
            dataTableOutput(
              #uiOutput(
              outputId = 'out_click_country_data'
            )
          )
        )
      )
      
    )
  )
)


server <- function(input, output, session) {
  #' 
  # Data
  alcohol_global_subset <- reactive({
    food_consumption |>
      filter(food_category == input$food_category) |>
      select(country, all_of(input$variable_all))
  })
  #' 
  ggplot_initial <- reactive({
    ggplot() + 
      geom_map(
        data = world_map_data, map = world_map_data,
        aes(#x = long, y = lat,
          group = group, map_id = region),
        fill = "white", color = "#7f7f7f", size = 0.5
      )
  })
  #' 
  output$out_world_plot <- renderPlot({
    ggplot_initial() + 
      geom_map(
        data = alcohol_global_subset(), map = world_map_data,
        aes(fill = .data[[input$variable_all]], map_id = country),
        color = "#7f7f7f", size = 0.5
      ) +  
      coord_map(
        "rectangular", lat0 = 0, 
        xlim = c(-180, 180), ylim = c(-60, 90)) +
      scale_fill_continuous(
        low = 'thistle2', high = 'darkred', guide = 'colorbar'
      ) + 
      scale_y_continuous(breaks = c())  + 
      scale_x_continuous(breaks = c())  + 
      #labs(fill = "", title = "") +
      theme_bw() + 
      theme(
        legend.direction = 'horizontal',
        legend.position = c(0.15, 0.2),
        legend.title = element_blank(),
        axis.title = element_blank()
      )
  })
  #
  output$out_click_country_data <- renderDT({#renderUI({
    req(input$plot_hover)
    #dataTableOutput("vals")
    # })
    # output$vals <- renderPrint({
    hover <- input$plot_hover
    x <- unique(
      nearPoints(world_map_data, hover, xvar = 'long', 'lat')$region
    )
    #print(x)
    subset(food_consumption, country %in% x) |>
      filter(food_category == input$food_category) |>
      select(country, all_of(input$variable_all)) |>
      datatable(
        options = list(
          border = '1px solid #ddd',
          dom = 't', #search and pagination hiding
          scrollX = TRUE # scroll across the columns when view reduced
        ),
        escape = FALSE,
        selection = list(mode = 'single', target = 'cell'),
        width = '100%', height = '100%'#table remains in container,
      )
  })
}

shinyApp(ui, server)