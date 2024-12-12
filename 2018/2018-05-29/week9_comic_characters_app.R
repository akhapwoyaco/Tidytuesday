library(readr)
library(shiny)
library(bslib)
library(vcd)
library(dplyr)
library(ggplot2)
library(stringr)
#' 
#' data
#' 
tuesdata <- tidytuesdayR::tt_load('2018-05-29')
week9_comic_characters <- tuesdata$week9_comic_characters |> 
# week9_comic_characters <- read_csv(
  # file = "week9_comic_characters.csv", show_col_types = FALSE) |> 
  mutate(
    id = str_replace_all(string = id, pattern = " Identity$", replacement = ''),
    align = str_replace_all(string = align, pattern = " Characters$", replacement = ''),
    eye = str_replace_all(string = eye, pattern = " Eyes$", replacement = ''),
    hair = str_replace_all(string = hair, pattern = " Hair$", replacement = ''),
    sex = str_replace_all(string = sex, pattern = " Characters$", replacement = ''),
    gsm = str_replace_all(string = gsm, pattern = " Characters$", replacement = '')
  )

#' 
my_vars = names(week9_comic_characters)[c(6:10, 11)]
print(my_vars)
#' 
ui <- fluidPage(
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
                 href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-05-29"),
          tags$br(),
          tags$code("tuesdata <- tidytuesdayR::tt_load('2018-05-29')"),
          tags$br(),
          tags$code("week9_comic_characters <- tuesdata$week9_comic_characters"),
          tags$br()),
        helpText(
          'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
          paste0(
            'The week9_comic_characters data contains ', dim(week9_comic_characters)[1], 
            ' observations, and ', dim(week9_comic_characters)[2], ' features on 
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
          tags$h4("Mosaic Plots: "),
          tags$li(
            'Add Resource for Mosaic Plot Interpretation and Download Button Functionality')
        )
      )
    ),
    
    tabPanel(
      title = "Compare Two Variables",
      fluidPage(
        fluidRow(
          column(3, 
                 radioButtons(
                   inputId = 'c_publisher1', 
                   label = "Filter By Publisher",
                   choices = list(
                     "Marvel", 
                     "DC"
                   ))),
          column(3,
                 selectInput(
                   inputId = 'var_11', label = 'Select Variable: ', 
                   choices = my_vars[1],
                   width = '50%'
                 )),
          column(3,
                 selectInput(
                   inputId = 'var_12', label = 'Select Variable: ', 
                   choices = my_vars[2], 
                   width = '50%'
                 ))),
        fluidRow(
          plotOutput(outputId = 'mosaic_plot_tab_1', 
                     height = '800px',width = '90%')),
        fluidRow(
          column(4, 
                 downloadButton(outputId = 'downloadPlot1',
                                label = 'DownLoadPlot1'))
        ))
    ),
    #' TAB 2
    tabPanel(
      title = "Compare Three Variables",
      fluidPage(
        fluidRow(
          column(3, 
                 radioButtons(
                   inputId = 'c_publisher2', 
                   label = "Filter By Publisher",
                   choices = list(
                     "Marvel", 
                     "DC"
                   ))),
          column(3,
                 selectInput(
                   inputId = 'var_21', label = 'Select Variable: ', 
                   choices = my_vars[1],
                   width = '50%'
                 )),
          column(3,
                 selectInput(
                   inputId = 'var_22', label = 'Select Variable: ', 
                   choices = my_vars[2], 
                   width = '50%'
                 )),
          column(3,
                 selectInput(
                   inputId = 'var_23', label = 'Select Variable: ', 
                   choices = my_vars[3],
                   width = '50%'
                 ))),
        fluidRow(
          plotOutput(outputId = 'mosaic_plot_tab_2', 
                     height = '800px',width = '90%')),
        fluidRow(
          column(4, 
                 downloadButton(outputId = 'downloadPlot2',
                                label = 'DownLoadPlot2'))
        ))),
    #' TAB 3
    tabPanel(
      title = "Compare Four Variables",
      fluidPage(
        fluidRow(
          column(3, 
                 radioButtons(
                   inputId = 'c_publisher3', 
                   label = "Filter By Publisher",
                   choices = list(
                     "Marvel", 
                     "DC"
                   ))),
          column(2,
                 selectInput(
                   inputId = 'var_31', label = 'Select Variable: ', 
                   choices = my_vars[1],
                   width = '50%'
                 )),
          column(2,
                 selectInput(
                   inputId = 'var_32', label = 'Select Variable: ', 
                   choices = my_vars[2], 
                   width = '50%'
                 )),
          column(2,
                 selectInput(
                   inputId = 'var_33', label = 'Select Variable: ', 
                   choices = my_vars[3],
                   width = '50%'
                 )),
          column(2,
                 selectInput(
                   inputId = 'var_34', label = 'Select Variable: ', 
                   choices = my_vars[4], 
                   width = '50%'
                 )
          )),
        fluidRow(
          plotOutput(outputId = 'mosaic_plot_tab_3', 
                     height = '800px',width = '90%')),
        fluidRow(
          column(4, 
                 downloadButton(outputId = 'downloadPlot3', 
                                label = 'DownLoadPlot3'))
        )
      )
    )
  )
)
server <- function(input, output, session) {
  #' 
  shiny::observe({
    #' TAB 1
    if (req(input$tabs) == "Compare Two Variables"){
      shiny::observe({
        if(!is.null(input$var_11))
          updateSelectInput(
            session = session, 
            inputId = 'var_12',
            choices = my_vars[!(my_vars %in% input$var_11 )],
            selected = isolate(input$var_12))
      })
      #
      shiny::observe({
        if(!is.null(input$var_12))
          updateSelectInput(
            session = session, 
            inputId = 'var_11',
            choices = my_vars[!(my_vars %in% input$var_12 )],
            selected = isolate(input$var_11))
      })
    }
    #' TAB 2
    if (req(input$tabs) == "Compare Three Variables"){
      shiny::observe({
        if(!is.null(input$var_22) & !is.null(input$var_23) )
          updateSelectInput(
            session = session, 
            inputId = 'var_21',
            choices = my_vars[!(my_vars %in% c(input$var_22, input$var_23 ) )],
            selected = isolate(input$var_21))
      })
      #
      shiny::observe({
        if(!is.null(input$var_21) & !is.null(input$var_23) )
          updateSelectInput(
            session = session, 
            inputId = 'var_22',
            choices = my_vars[!(my_vars %in% c(input$var_21, input$var_23 ) )],
            selected = isolate(input$var_22))
      })
      #
      shiny::observe({
        if(!is.null(input$var_21) & !is.null(input$var_22) )
          updateSelectInput(
            session = session, 
            inputId = 'var_23',
            choices = my_vars[!(my_vars %in% c(input$var_21, input$var_22 ) )],
            selected = isolate(input$var_23))
      })
    }
    #' TAB 3
    if (req(input$tabs) == "Compare Four Variables"){
      shiny::observe({
        if(!is.null(input$var_32) & !is.null(input$var_33) & !is.null(input$var_34))
          updateSelectInput(
            session = session, 
            inputId = 'var_31',
            choices = my_vars[!(my_vars %in% c(input$var_32, input$var_33, input$var_34) )],
            selected = isolate(input$var_31))
      })
      #
      shiny::observe({
        if(!is.null(input$var_31) & !is.null(input$var_33) & !is.null(input$var_34))
          updateSelectInput(
            session = session, 
            inputId = 'var_32',
            choices = my_vars[!(my_vars %in% c(input$var_31, input$var_33, input$var_34) )],
            selected = isolate(input$var_32))
      })
      #
      shiny::observe({
        if(!is.null(input$var_31) & !is.null(input$var_32) & !is.null(input$var_34))
          updateSelectInput(
            session = session, 
            inputId = 'var_33',
            choices = my_vars[!(my_vars %in% c(input$var_31, input$var_32, input$var_34) )],
            selected = isolate(input$var_33))
      })
      #
      shiny::observe({
        if(!is.null(input$var_31) & !is.null(input$var_32) & !is.null(input$var_33))
          updateSelectInput(
            session = session, 
            inputId = 'var_34',
            choices = my_vars[!(my_vars %in% c(input$var_31, input$var_32, input$var_33) )],
            selected = isolate(input$var_34))
      })
    }
  })
  #'
  #'
  data_plot <- reactive({
    week9_comic_characters |> 
      dplyr::select(publisher, all_of(my_vars)) |> 
      na.omit() 
  })
  #' TAB 1
  #' Data
  data_plot1 <- reactive({
    data_plot() |>
      filter(publisher == input$c_publisher1) |>
      select(input$var_11, input$var_12)
  })
  #' Output
  # mosaic_plot_tab_11 <- reactive({
  #   formula_all = as.formula(
  #     paste("~", paste(
  #       input$var_11, input$var_12, sep = "+")))
  #   mosaic(formula = formula_all, data = data_plot1(), 
  #          labeling = labeling_values,
  #          zero_size = 0,
  #          rot_labels = c(0,0,0,0), 
  #          shade = TRUE, legend = TRUE)
  # })
  #' 
  output$mosaic_plot_tab_1 <- renderPlot({
    formula_all = as.formula(
      paste("~", paste(
        input$var_11, input$var_12, sep = "+")))
    mosaic(formula = formula_all, data = data_plot1(), 
           labeling = labeling_values,
           zero_size = 0,
           rot_labels = c(0,0,0,0), 
           shade = TRUE, legend = TRUE)
  })
  # Add download button
  # output$downloadPlot1 <- downloadHandler(
  #   filename = function(){
  #     paste(
  #       paste(input$var_11, input$var_12, sep = '_'),
  #       '.png', sep = ''
  #     )},
  #   content = function(file){
  #     png(filename = file, width = '25', height = '20', 
  #         units = 'cm', res = 600)
  #     mosaic_plot_tab_11()
  #     dev.off()
  #   }
  # )
  
  # TAB 2
  # Data
  data_plot2 <- reactive({
    data_plot() |> 
      filter(publisher == input$c_publisher2) |>
      select(input$var_21, input$var_22, input$var_23) 
  })
  #' Output
  output$mosaic_plot_tab_2 <- renderPlot({
    formula_all = as.formula(
      paste("~", paste(
        input$var_21, input$var_22,  input$var_23,sep = "+")))
    mosaic(formula = formula_all, data = data_plot2(), 
           labeling = labeling_values,
           zero_size = 0,
           rot_labels = c(0,0,0,0), 
           shade = TRUE, legend = TRUE)
  })
  
  #' TAB 3
  #' Data
  data_plot3 <- reactive({
    data_plot() |> 
      filter(publisher == input$c_publisher3) |>
      select(input$var_31, input$var_32, input$var_33, input$var_34) 
  })
  #' Output
  output$mosaic_plot_tab_3 <- renderPlot({
    formula_all = as.formula(
      paste("~", paste(
        input$var_31, input$var_32, input$var_33, input$var_34, 
        sep = "+")))
    # do.call('mosaic', list(formula = formula_all, data = data_plot2()))
    mosaic(formula = formula_all, data = data_plot3(), 
           labeling = labeling_values,
           zero_size = 0,
           rot_labels = c(0,0,0,0), 
           shade = TRUE, legend = TRUE)
  })
}

shinyApp(ui, server)