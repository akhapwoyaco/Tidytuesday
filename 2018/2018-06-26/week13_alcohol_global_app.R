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
tuesdata <- tidytuesdayR::tt_load('2018-06-26')
week13_alcohol_global <- tuesdata$week13_alcohol_global

# week13_alcohol_global <- read_csv(
#   file = "week13_alcohol_global.csv",
#   show_col_types = FALSE)
my_vars = names(week13_alcohol_global[sapply(week13_alcohol_global, is.numeric)])
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
  header = dashboardHeader(title = 'Global Alcohol', titleWidth = 250),
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
                     href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-06-26"),
              tags$br(),
              tags$code("tuesdata <- tidytuesdayR::tt_load('2018-06-26')')"),
              tags$br(),
              tags$code("week13_alcohol_global <- tuesdata$week13_alcohol_global")),
            
            helpText(
              'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
              paste0(
                'The data contains ', dim(week13_alcohol_global)[1], 
                'observations, on states within across the globe, with ', dim(week13_alcohol_global)[2], 
                ' features'))),
        box(title = 'Shiny App Guide', status = 'success', width = 12,
            height = 300, solidHeader = TRUE, 
            helpText(
              # include the html for lista and bold text
              tags$h4('Introduction:'),
              tags$li(
                'Source of data is explained, along with period the data covers.'),
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
            title = NULL, #"Variable", 
            status = 'success', width = 12,
            solidHeader = TRUE, height = 50,
            column(width = 12, align = 'center',
                   radioButtons(
                     inputId = 'variable_all', label = NULL,#'Select Variable: ', 
                     choices = my_vars,# width = '50%',
                     inline = TRUE))),
          br(),
          box(
            title = NULL,#"World Plot", 
            status = 'success', width = 12,
            solidHeader = TRUE, height = 450,
            plotOutput(
              outputId = 'out_world_plot',
              hover = hoverOpts(id = 'plot_hover', delay = 0.00005),
              width = '90%', height = 400)),
          box(
            title = NULL,#"Country Data", 
            status = 'success', width = 12,
            solidHeader = TRUE, height = 300,
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
  #' Data
  alcohol_global_subset <- reactive({
    week13_alcohol_global |> 
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
    subset(week13_alcohol_global, country %in% x) |>
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