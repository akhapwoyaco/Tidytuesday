library(readxl)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)  

#' get data
tuesdata <- tidytuesdayR::tt_load('2018-04-16')
global_mortality <- tuesdata$global_mortality
mortality_cause_all = names(global_mortality)[-c(1,2,3)]

#' some JS function to show NA values in tables
#' source https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny
rowCallback <- c(
  "function(row, data){",
  " for (var i = 0; i < data.length; i++){",
  "  if (data[i] === null){",
  "   $('td:eq('+i+')', row).html('NA')",
  "    .css({'color': 'rgb(151, 151, 151)', 'font-style': 'italic'});",
  "   }",
  " }",
  "}"
)
#' 



#'UI
ui <- dashboardPage(
  skin = 'blue',
  #Dashboard title
  header = dashboardHeader(title = 'Mortality Analysis', titleWidth = 250),
  
  #Sidebar layout
  sidebar = dashboardSidebar(
    width = 120,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = 'data_description',
               icon = icon('gear')),
      menuItem("Plots", tabName = 'data_plots', icon = icon('bar-chart'))
      , actionButton('switchtab', 'Switch Tab')) 
  ), # happens on clicking another tab
  
  #Tabs 
  
  body = dashboardBody(
    
    tags$head(tags$style(HTML(
      '.main-header .logo {font-weight: bold;} .box-body {font-size: 11px}'
    ))),
    tabItems(
        tabItem(
          tabName = 'data_description',
          fluidPage(
          box(title = 'Data Source', status = 'success', width = 12,
              height = 180, solidHeader = TRUE, 
              helpText(
                'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-02, and as such we not 
              hold the right to the data: ', 
                tags$a("TidyTuesday data 2018-04-16", 
                       href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-16"),
                tags$br(),
                tags$code("tuesdata <- tidytuesdayR::tt_load('2018-04-16')"),
                tags$br(),
                tags$code("global_mortality <- tuesdata$global_mortality")),
              
              helpText(
                'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
                paste0(
                  'The global mortality data contains ', dim(global_mortality)[1], ' observations, 
            on various countries across the globe over varying durations, with ', 
                  dim(global_mortality)[2], ' features'))),
          
          box(title = 'Data Period', status = 'success', width = 12,
              height = 150, solidHeader = TRUE,
              dataTableOutput(outputId = 'mortality_period')
          ),
          box(title = 'Shiny App Guide', status = 'success', width = 12,
              height = 350, solidHeader = TRUE, 
              helpText(
                # include the html for lista and bold text
                tags$h4('Introduction:'),
                tags$li(
                  'Source of data is explained, along with period the data covers.'),
                tags$h4("Plots: "),
                tags$li(
                  'Provides the Base R and GGplot2 plots, as well as data on the yearly
                percentages per cause of mortality by Country in comparison to the 
                global scale.'),
                tags$h4("Interesting Stuffs: "),
                tags$li(
                  'The inclusion of NAs in data tables in place of empty cells.'),
                tags$li(
                  'Plotting vector with all NA variables in base R'),
                tags$li(
                  'Transpose dataframe and use column 1 as colnames, though 
                differences in the use of %>% and |> (failed to work) arise.'),
                tags$li(
                  'Switch tabs button'),
                tags$h4("References: "),
                tags$li(
                  tags$a("stackoverflow: customizing how datatables displays missing values in shiny"),
                  href = 'https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny')
              )
          )
        )
        
      ),
      
      tabItem(
        tabName = 'data_plots',
        fluidPage(
          fluidRow(
            box(
              solidHeader = TRUE, title = "Select Country: ",
              status = 'success', width = 6, height = 120,
              selectInput(inputId = 'country_name', label = 'Country',
                          choices = unique(global_mortality$country))),
            box(
              solidHeader = TRUE, title = "Select Mortality Cause: ",
              status = 'success', width = 6, height = 120,
              selectInput(inputId = 'mortality_cause', label = 'Mortality Cause',
                          choices = mortality_cause_all))
          ),
          
          box(
            title = "Mortality",
            status = 'success',
            solidHeader = TRUE, width = 12, height = 210,
            dataTableOutput(outputId = 'country_data')),
          
          box(
            title = "",
            status = 'success',
            solidHeader = TRUE, width = 6,
            plotOutput(outputId = 'country_plot_base')
          ), 
          box(
            title = "",
            status = 'success',
            solidHeader = TRUE, width = 6,
            plotOutput(outputId = 'country_plot_ggplot2')
          )
        )
        
      )
    )
  )
)

#' server
#' 
#' 
server <- function(input, output, session) {
  # clicking on another tab
  observeEvent(
    eventExpr = input$switchtab,
    handlerExpr = {
      newtab <- switch(
        input$tabs,
        'data_description' = 'data_plots',
        'data_plots' = 'data_description')
      updateTabItems(session, "tabs", newtab)})
  
  # uniwue year
  year_unique <- reactive({
    unique(global_mortality$year)
  })
  
  # Data Period Covered
  output$mortality_period <- renderDT(
    year_unique() |> 
      matrix(ncol = 9, nrow = 3) |> 
      as.data.frame(optional = TRUE)|>
      datatable(options = list(
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
      width = '100%', height = '100%'#table remains in container,
      )  
  )
  
  #' tab Item 2
  #' global mortality long format
  
  global_mortality_long <- reactive({
    global_mortality |> 
      select(-country_code) |>
      pivot_longer(
        cols = contains('%'),
        values_to = "Percent", names_to = "Cause") 
  })
  
  country_mort_data <- reactive({
    global_mortality_long() |> 
      filter(country == input$country_name) |> 
      filter(Cause == input$mortality_cause) |> 
      select(-country, -Cause)
  })
  
  world_mort_data <- reactive({
    global_mortality_long() |> 
      filter(country == "World") |> 
      filter(Cause == input$mortality_cause) |> 
      select(-country, -Cause)
  })
  
  country_mort_cause <- reactive({
    country_mort_data() |>
      t() |> as.data.frame() %>% # weird that |> does not work here
      `colnames<-`(.[1,]) %>% 
      .[-1,] |>
      round(2)
  })
  # used for plotting
  world_country_mort_cause <- reactive({
    country_mort_data() |>
      left_join(world_mort_data(), by = 'year') %>% 
      setNames(nm = c("year", input$country_name, 'World')) 
  })
  #'Goes to table
  world_country_mort_cause_v2 <- reactive({
    world_country_mort_cause() |> 
      t() |> as.data.frame() %>% 
      # weird that |> does not work from previous code here onward 
      `colnames<-`(.[1,]) %>% 
      .[-1,] |> round(2)
  })
  #
  #' Time Series Plot of each Country's Mortality Trends
  #' 
  output$country_data <- renderDT({
    world_country_mort_cause_v2() |> 
      as.data.frame(row.names = c(input$country_name, "World")) |> 
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        dom = 't', #search and pagination hiding
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = TRUE) 
  })
  
  #' Plot
  output$country_plot_base <- renderPlot({
    x_val = country_mort_data()$year
    y_val = country_mort_data()$Percent
    y_min = min(
      c(
        y_val,
        as.vector(as.matrix(world_country_mort_cause()[, 2:3]))),
      na.rm = T
    )
    y_max = max(
      c(
        y_val, 
        as.vector(as.matrix(world_country_mort_cause()[, 2:3]))), 
      na.rm = T)
    #'
    #' some country data are NA for all sequence, returninf Inf, as such
    #' plot is empty, with ylimits set to minimum and maximum
    y_min <- ifelse(!is.finite(y_min), 0, y_min)
    y_max <- ifelse(!is.finite(y_max), 1, y_max)
    #
    col_names_data = colnames(world_country_mort_cause()[,-1])
    # print(y_min); print(y_max)
    plot(x = x_val, y = y_val,
         ylab = 'Percent', xlab = 'Year',
         col = 'blue', type = 'b', pch = 19,
         col.lab = 'black', cex.lab = 1.2, las = 1,
         ylim = c(y_min, y_max),
         main = paste(
           input$country_name, ':', input$mortality_cause, "Mortality",
           sep = ' '))
    lines(x = x_val, y = world_country_mort_cause()$World, type = 'b', 
          col = 'red', pch = 5)
    legend('top', bty = 'n', lty = 2, inset = c(0, -0.1), xpd = TRUE, 
           legend = c(input$country_name, 'World'), ncol = 2,
           col = c('blue', 'red'), pch = c(19, 5))
    grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  })
  output$country_plot_ggplot2 <- renderPlot({
    world_country_mort_cause() %>% 
      pivot_longer(
        cols = !year, values_to = "Percent",
        names_to = "Locality"
      ) |>
      ggplot(aes(x = year, y = Percent, #group = 1, 
                 color = Locality)) + 
      geom_line() + geom_point() +   
      scale_color_manual(
        values = c(
          "blue",'red')
      ) +
      labs(y = 'Percent', x = 'Year',
           title = paste(
             input$country_name, ':', input$mortality_cause, "Mortality",
             sep = ' ')) + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
            legend.position = "top", legend.title = element_blank())
  })
  
  #' 
  #' 
  #' 
}

shinyApp(ui, server)

