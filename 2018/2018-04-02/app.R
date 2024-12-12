## usa_states_average_tuition
library(tidytuesdayR)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(maps)
library(devtools)
#' Import Data
#' read in the data manually
tuesdata <- tidytuesdayR::tt_load('2018-04-02')
us_avg_tuition <- tuesdata$us_avg_tuition
state_admission_rates = read_csv(
  file = 'data/state_admission_rate_avg.csv', show_col_types = FALSE)
#' 


#R Shiny ui
ui <- dashboardPage(
  skin = 'purple',
  #Dashboard title
  header = dashboardHeader(title = 'Table', titleWidth = 250),
  
  #Sidebar layout
  sidebar = dashboardSidebar(
    width = 120,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = 'data_description',
               icon = icon('gear')),
      menuItem("Data", tabName = 'data_tab', icon = icon('table')),
      menuItem("Plots", tabName = 'data_plots', icon = icon('bar-chart'))
      , actionButton('switchtab', 'Switch Tab')) 
  ), # happens on clicking another tab
  
  #Tabs 
  
  body = dashboardBody(
    
    tags$head(tags$style(HTML(
      '.main-header .logo {font-weight: bold;} .box-body {font-size: 11px}'
    ))),
    
    # FIRST ITEM 
    tabItems(
      
      # THE VERY FIRST ITEM on DATA SOURCE
      tabItem(
        tabName = 'data_description',
        box(title = 'Data Source', status = 'success', width = 12,
            height = 200, solidHeader = TRUE, 
            helpText(
              'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-02, and as such we not 
              hold the right to the data content: ', 
              tags$a("TidyTuesday data 2018-04-02", 
                     href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-02"),
              tags$br(),
              tags$code("tuesdata <- tidytuesdayR::tt_load('2018-04-02')"),
              tags$br(),
              tags$code("us_avg_tuition <- tuesdata$us_avg_tuition")),
            
            helpText("The college admission rates data was obtained from ", 
                     tags$a("U.S. DEPARTMENT OF EDUCATION: College Scorecard", 
                   href = "https://collegescorecard.ed.gov/data"), "
                   and necessary columns derived through code in file: "),
            helpText(
              'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
            paste0(
            'The tuition data containes ', dim(us_avg_tuition)[1], ' observations, 
            for each State in the United States of America, and ', 
            dim(us_avg_tuition)[2], ' features on States and Average 
            Tuition Fees for specific Periods. The college data only 
            makes use of STATE and Admission Rates Columns derived from each of the 
            merged datasets')),
            helpText("I don't think there was any patterns between the admission 
                     rates and the average tuition fees, but hope in future to 
                     incoporate other factors per state and make worthy conclusions.")),
        
        box(title = 'Data Period', status = 'success', width = 12,
            height = 150, solidHeader = TRUE,
            dataTableOutput(outputId = 'tuition_period')
            ),
        box(title = 'SideBar Tab Guides', status = 'success', width = 12,
            height = 250, solidHeader = TRUE, 
            helpText(
              # include the html for lista and bold text
                tags$h4('Data:'),
                tags$li(
                'Average Tuition by State across all periods (row-wise) 
      are calculated and compared to each States average 
      tuition across the row for corresponding State, with those 
      tuition above the State average being dark shaded while those 
      below being shaded lightly.'),
                tags$li(
                'Average Tuition by Period across all States (column-wise) 
      are calculated and compared to each Period average 
      tuition across the column for corresponding Period, with those 
      tuition above the Period average being dark shaded while those 
      below being shaded lightly.'),
                tags$li(
                  'Selection is made by the radio buttons on top right box.'),
                
                tags$h4("Plots: "),
                tags$li(
                  'Average Admission Rates per Academic Period for each
                  State were calculated by averaging the admission rates 
                  also called the acceptance rates.
                  (ADM_RATE includes the admissions rate at each campus).')
                        
               )
            
        )
        
        ) ,
      # Data and Averages
      tabItem(
        tabName = 'data_tab', 
        
        fluidPage(
          
          # Empty BOX
          box(title = NULL, background = 'olive', width = 1, height = 150),
          
          # Average tuition per year
          box(title = 'Average Tuition: Per Academic Period', 
              status = 'success', 
              solidHeader = TRUE, width = 9, height = 150,
              dataTableOutput(outputId = 'aveg_tuition_acad_per')), 
          
          # EMPTY BOX
          box(title = NULL, background = 'olive', width = 2, height = 150, 
              radioButtons(
                inputId = 'mean_comparison', 
                label = "Compare Average Tuition Fee",
                choices = list(
                  "All States for Individual Academic Year" = 'by_period', 
                  "All Year for Individual State" = 'by_state'
                )
              )),
          
          # Data tab
          box(
            title = 
              'USA Average Tuition Fees: Darker regions imply higher tuition fees than associated average', 
              status = 'primary', width = 10,
              solidHeader = TRUE, #height = 1100,
              dataTableOutput(outputId = 'aveg_tuition')),
          
          # Display Average Tuition Per State
          box(title = 'Average Tuition', status = 'success', 
              width = 2, #height = 1100, #1000
              solidHeader = TRUE,
              dataTableOutput(outputId = 'aveg_tuition_state'))
        )
      )
      ,
      
      # Next Section ITEM
      tabItem(
        tabName = 'data_plots',
        fluidPage(
          fluidRow(
            box(
              solidHeader = TRUE, title = "Select State: ",
              status = 'success', width = 5, height = 150,
              selectInput(inputId = 'us_state', label = 'State',
                          choices = unique(us_avg_tuition$State))),
            box(
              solidHeader = TRUE, title = NULL,
              width = 6, height = 250,
              status = 'success', 
              plotOutput(outputId = 'state_plot')
              )
            ),
          
  
          box(
              title = "Average Tuition Fee per Academic Calendar",
              status = 'success',
              solidHeader = TRUE, width = 12, height = 210,
              dataTableOutput(outputId = 'state_data')),
          
          
          box(
            title = "Average Admission Rates per Academic Calendar",
            status = 'success',
            solidHeader = TRUE, width = 12, height = 170,
            dataTableOutput(outputId = 'state_data_admission')),
          
          
          box(
            title = "",
            status = 'success',
            solidHeader = TRUE, width = 6,
            plotOutput(outputId = 'state_plot_base')
          ), 
          box(
            title = "",
            status = 'success',
            solidHeader = TRUE, width = 6,
            plotOutput(outputId = 'state_plot_ggplot2')
          )
          
        )
      ) 
    )
  )
)


server <- function(input, output, session) {
  # TAB 1
  
  output$tuition_period <- renderDT(
    colnames(us_avg_tuition)[-1] |> 
      matrix(ncol = 4) |> 
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
  # clicking on another tab
  observeEvent(
    eventExpr = input$switchtab,
    handlerExpr = {
      newtab <- switch(
        input$tabs,
        'data_description' = 'data_tab',
        'data_tab' = 'data_plots',
        'data_plots' = 'data_tab')
      updateTabItems(session, "tabs", newtab)})
  
  #
  # TAB ITEM DATA
  aveg_tuition_acad_period <- reactive({
    #' means per year
    colMeans(us_avg_tuition[,-1]) |> t()
  })
  
  aveg_tuition_by_state <- reactive({
    #' means per state
    per_state_means <- rowMeans(us_avg_tuition[,-1])
    names(per_state_means) <- us_avg_tuition$State
    per_state_means <- per_state_means |> as.data.frame()
    colnames(per_state_means) <- c("Means")
    per_state_means$State <- rownames(per_state_means)
    rownames(per_state_means) <- NULL
    per_state_means$State <- state.abb[match(per_state_means$State, state.name)]
    # per_state_means <- per_state_means[,c(2,1)]
    per_state_means
  })
  
  output$aveg_tuition_acad_per <- renderDT({
    aveg_tuition_acad_period() |> as.data.frame() |> 
      datatable(options = list(
        border = '1px solid #ddd',
        dom = 't', #search and pagination hiding
        scrollX = TRUE # scroll across the columns when view reduced
      ), width = '100%', height = '100%', #table remains in container,
      rownames = FALSE) |>
      formatCurrency(c(1:12), digits = 0)
  })
  output$aveg_tuition_state <- renderDT({
    aveg_tuition_by_state() |> 
      datatable(options = list(
        lengthChange = FALSE, #remove that Show entries dropdown
        #dom = 't', #ft
        scrollX = TRUE, autoWidth = FALSE,
        pageLength = 50 ), 
        width = '100%', height = '100%', #table remains in container, 
        rownames = FALSE) |>
      formatCurrency(c("Means"), digits = 0)
  })
  
  data_to_display <- eventReactive(
    input$mean_comparison,
    {
      # each cell value is compared to the period average or per state average
      if (input$mean_comparison == "by_period"){
        # By yearly Mean
        usa_colors <- sweep(us_avg_tuition[,-1], 2, 
                            aveg_tuition_acad_period(), "<")
        usa_formatted <- cbind(us_avg_tuition, usa_colors)
        usa_formatted
      } else {
        # by state mean
        usa_colors <- sweep(us_avg_tuition[,-1], 1,
                            aveg_tuition_by_state()[,1], '<')
        usa_formatted <- cbind(us_avg_tuition, usa_colors)
        usa_formatted
      }
    }
    )

  output$aveg_tuition <- renderDT({
    cols_keep <- 2:13
    cols_color <- 14:25
    #
    datatable(
      data_to_display(),#usa_formatted, 
      options = list(
        #dom = 'ft', 
        # TODO have the full table or paginated table with next, but without number of entries
        scrollX = TRUE,
        lengthChange = FALSE, #remove that Show entries dropdown
        pageLength = 50,#info = TRUE,
        columnDefs = list(list(targets = cols_color, 
                               visible = FALSE))
      ), width = '100%', height = '100%' #table remains in container
      ) %>%
      formatStyle(
        cols_keep,
        cols_color,
        backgroundColor = styleEqual(c(0,1), c("#4292C6", "#9ECAE1"))
      ) |> 
      formatCurrency(c(2:13), digits = 0) 
    
  })
  
  
  # TAB ITEM DATA PLOTS
  # Reactive state to capture input state data
  us_data <- reactive({
    subset(x = us_avg_tuition, subset = State == input$us_state,
           select = -State)
  })

  # state_plot
  output$state_plot <- renderPlot({
    name_vec <- map(
      database = 'state', col = 'blue',
      fill = TRUE, namesonly = TRUE)
    #
    map(database = 'state',
        col = c("white", 'red')[1+(name_vec %in% tolower(input$us_state))],
        fill = TRUE)
  }, height = 200)

  us_adm_rates = reactive({
    state_admission_rates |> 
      subset(STABBR == input$us_state) |>
      pivot_wider(
        id_cols = STABBR, names_from = year, 
        values_from = average_admission_rates
      ) |> 
      tibble::column_to_rownames(var = "STABBR")
  })
  #
  us_adm_rates_avg = reactive({
    state_admission_rates |> 
      subset(STABBR == input$us_state)
    })
  #
  output$state_data_admission <- renderDT({
    us_adm_rates() |>
      datatable(
        options = list(dom = 't', scrollX = TRUE),
        width = '100%', height = '100%' #table remains in container
      ) |>
      formatRound(1:27, digits = 2)
  })
  # Output data per state
  output$state_data <- renderDT({
    us_data() |> as.data.frame() |> 
      rbind(aveg_tuition_acad_period()) |> 
      as.data.frame(
        row.names = c("State", "All States Average")) |>
      datatable(
        options = list(dom = 't', scrollX = TRUE),
        width = '100%', height = '100%' #table remains in container
        ) |>
      formatCurrency(c(1:12), digits = 2)
  })
  # Plot output
  output$state_plot_base <- renderPlot({
    x_val = 1:ncol(us_data())
    y_val = t(us_data())
    y_min = min(c(y_val, aveg_tuition_acad_period()))
    y_max = max(c(y_val, aveg_tuition_acad_period()))
    col_names_data = colnames(us_data())
    plot(x = x_val, y = y_val,
         ylab = 'Average Tuition', xlab = '',
         col = 'blue', type = 'b', pch = 19,
         col.lab = 'black', cex.lab = 1.2, las = 1,
         xaxt = 'n', ylim = c(y_min, y_max),
         main = paste(
           'United States of America:', input$us_state, 'Average Tuition',
           sep = ' '))
    lines(x = x_val, y = aveg_tuition_acad_period(), type = 'b', 
          col = 'red', pch = 5)
    axis(1, at = x_val, labels = col_names_data, las = 2)
    legend('topleft', bty = 'n', lty = 2,
           legend = c(input$us_state, 'All States Average'),
           col = c('blue', 'red'), pch = c(19, 5))
    grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  })
  output$state_plot_ggplot2 <- renderPlot({
    us_adm_rates_avg() |>
      #mutate(year = factor(year, ordered = T)) %>%
      ggplot(aes(x = year, y = average_admission_rates, group = 1)) +
      geom_point(color = 'blue') + geom_line() +
      labs(y = 'Average Admission Rates', x = '',#''Year',
           title = paste(
             'United States of America:', input$us_state, 
             'College Admission Rates',
             sep = ' ')) +
      theme_bw() + scale_y_continuous(limits = c(0,1)) + 
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
            axis.text = element_text(angle = 90))
  })
}

shinyApp(ui, server)