library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(bslib)
library(reshape2)

tuesdata <- tidytuesdayR::tt_load('2018-05-21')
honey_production <- read.csv('honeyproduction.csv')
#head(honeyproduction)
#'date column to as.datee
honey_production$year <- as.Date(
  x = paste(honey_production$year, 12, 31, sep = '-'))
#' get numeric variables
my_vars = names(honey_production[sapply(honey_production, is.numeric)])
#
honey_production$state <- state.name[match(honey_production$state, state.abb)]
#
ui <- fluidPage(
  theme = bs_theme(
      bg = "#101010", 
      fg = "#FDF7F7", 
      primary = "#ED79F9", 
      base_font = font_google("Prompt"),
      code_font = font_google("JetBrains Mono")
    ),
  tabsetPanel(
    tabPanel(
      title = "Introduction",
      fluidPage(
        helpText(
          'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-02, and as such we not 
              hold the right to the data content: ', 
          tags$a("TidyTuesday data 2018-04-02", 
                 href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-05-21"),
          tags$br(),
          # tags$code("tuesdata <- tidytuesdayR::tt_load('2018-05-21')"),
          # tags$br(),
          # tags$code(""),
          tags$br()),
        helpText(
          'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
          paste0(
            'The honey production data contains ', dim(honey_production)[1], 
            ' observations, and ', dim(honey_production)[2], ' features on 
                various comic charactesr.')),
        helpText("Worthy conclusion can be made with regard to the honey production
        variable plots across multiple states and over the course of time."),
        helpText(
          # include the html for lista and bold text
          tags$h4('Data:'),
          tags$li(
            "Selection of states and variables is made by the drop downs."), 
          tags$li("Select Inputs define the features to get 
            associations by"),
          tags$h4("Plots: "),
          tags$li(
            'Add Resource for Mosaic Plot Interpretation and Download Button Functionality')
        )
      )
    ),
    tabPanel(
      title = "Compare Variable Over Time: ",
      fluidRow(
        selectInput(
          inputId = 'usa_state', label = 'Select State: ', 
          choices = unique(honey_production$state), width = '50%'
          #multiple = TRUE, size = 2, selectize = FALSE
        ),
        selectInput(
          inputId = 'usa_state_var_y', label = 'Select Variable: ', 
          choices = names(honey_production[,-c(1, 8)]), width = '50%'
          #multiple = TRUE, size = 2, selectize = FALSE
        )
      ),
      fluidRow(
        plotOutput(outputId = 'honey_plot', width = '90%', height = '760px')),
      fluidRow(
        tableOutput(outputId = 'usa_state_table'))
    ),
    tabPanel(
      title = "Compare Variables against each other",
      fluidRow(
        column(3,
        selectInput(
          inputId = 'usa_state_tab_2', label = 'Select State: ', 
          choices = unique(honey_production$state), width = '50%')
        ),
        column(3,
               selectInput(
          inputId = 'usa_state_tab_2_var_y', label = 'Select Variable Y: ', 
          choices = my_vars[1],
          width = '50%'
        )),
        column(3,
          selectInput(
          inputId = 'usa_state_tab_2_var_x', label = 'Select Variable X: ', 
          choices = my_vars[2], 
          width = '50%'
        )
      )),
      fluidRow(
        column(6, 
        plotOutput(outputId = 'honey_plot_tab_2', width = '100%', height = '760px')),
        column(6, 
        plotOutput(outputId = 'usa_states_corr_2', width = '100%', height = '760px'))),
      fluidRow(
        tableOutput(outputId = 'usa_state_table_tab_2'))
    )
  )
)

server <- function(input, output, session) {
  # TabPanel 1
  all_state_data <- reactive({
    honey_production |> 
      select(state, year, any_of(input$usa_state_var_y))
  })
  
  select_state_data <- reactive({
    req(input$usa_state_var_y)
    all_state_data() |> 
      filter(state == input$usa_state)
    #select(state, year, .data[[input$usa_state_var_y]])
  })
  # 
  output$honey_plot <- renderPlot({
    all_state_data() |>
      ggplot(aes(x = year, y = .data[[input$usa_state_var_y]])) +
      geom_point(alpha = 0.2, color = 'black') + 
      geom_point(data = select_state_data(), 
                 aes(x = year, y = .data[[input$usa_state_var_y]]), 
                 color = 'red') +
      theme_bw() + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(labels = comma)
  })
  output$usa_state_table <- renderTable({
    select_state_data() |> 
      mutate(year = as.numeric(substr(year, 1, 4))) |>
      select(-state) |> t() |> 
      as.data.frame() %>% 
      `colnames<-`(.[1,]) %>% 
      .[-1,]
  }, striped = TRUE, bordered = TRUE, rownames = TRUE, 
  digits = 1, na = "NA", width = '70%')
  
  #' 
  #' Tab Panel 2
  #' We want to have x axis option not available for y axis option
  #' to avoid plotting a variable against itself
  #' 
  shiny::observe({
    if(!is.null(input$usa_state_tab_2_var_y))
      updateSelectInput(
        session = session, 
        inputId = 'usa_state_tab_2_var_x',
        choices = my_vars[!(my_vars %in% input$usa_state_tab_2_var_y )],
        selected = isolate(input$usa_state_tab_2_var_x))
  })
  #
  shiny::observe({
    if(!is.null(input$usa_state_tab_2_var_x))
      updateSelectInput(
        session = session, 
        inputId = 'usa_state_tab_2_var_y',
        choices = my_vars[!(my_vars %in% input$usa_state_tab_2_var_x )],
        selected = isolate(input$usa_state_tab_2_var_y))
  })
  #
  all_state_data_2 <- reactive({
    req(input$usa_state_tab_2_var_y)
    req(input$usa_state_tab_2_var_x)
    honey_production |> 
      filter(state == input$usa_state_tab_2) |> 
      select(year, state,
             any_of(input$usa_state_tab_2_var_y), 
             any_of(input$usa_state_tab_2_var_x)) |>
      mutate(year = as.numeric(substr(year, 1, 4)))
  })
  #
  output$honey_plot_tab_2 <- renderPlot({
    honey_production |> 
      ggplot(aes(x = .data[[input$usa_state_tab_2_var_x]],
                 y = .data[[input$usa_state_tab_2_var_y]])) + 
      geom_point(color = 'blue3') + 
      scale_y_continuous(labels = comma) + 
      scale_x_continuous(labels = comma) +
      geom_point(data = all_state_data_2(), 
                aes(x = .data[[input$usa_state_tab_2_var_x]],
                    y = .data[[input$usa_state_tab_2_var_y]]), 
                 color = 'red') +
      theme_bw() 
  })
  
  cor_mat <- reactive({
    cor_mat_honey_prod <- honey_production |> 
      select(any_of(my_vars)) |> 
      cor() |> round(2)
    # lower matrix triangle
    lower_tri <- cor_mat_honey_prod
    lower_tri[lower.tri(lower_tri)] <- NA 
    # 
    melt(lower_tri, na.rm = TRUE)
  })
  output$usa_states_corr_2 <- renderPlot({
    cor_mat() |> 
    ggplot(aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed() +
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
  })
  
  output$usa_state_table_tab_2 <- renderTable({
    all_state_data_2() |> select(-state) |> t() |>
      as.data.frame() %>% 
      `colnames<-`(.[1,]) %>% 
      .[-1,]
  }, striped = TRUE, bordered = TRUE, rownames = TRUE, 
  digits = 1, na = "NA", width = '60%')
  
}

shinyApp(ui, server)