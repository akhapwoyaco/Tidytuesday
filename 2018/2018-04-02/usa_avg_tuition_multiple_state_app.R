#' 
#' Shiny app to display line plots of each state to note trends
#' in both ggplot and base R
#'
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# #
tuesdata <- tidytuesdayR::tt_load('2018-04-02')
us_avg_tuition <- tuesdata$us_avg_tuition
# #
library(readr)
# us_avg_tuition <- readxl::read_xlsx("us_avg_tuition.xlsx")
#
ui <- fluidPage(
  titlePanel('Average Tuition USA', windowTitle = T),
  mainPanel(
    fluidRow(
      column(
        width = 6, 
        selectizeInput(
          inputId = 'us_state', label = 'State', 
          choices = unique(us_avg_tuition$State), 
          selected = us_avg_tuition$State[1],
          # used selectizeInput and maximum items to limit cluttering of plot
          multiple = T, options = list(maxItems = 4)
        )
      ),
      column(
        width = 6, 
        downloadButton(outputId = "download_image", label = "Download Image")
      )
    ),
    tableOutput(outputId = 'state_data'),
    #splitLayout(#cellWidths = c(450,450), 
    #  style = 'border: 1px solid silver:',
    #plotOutput(outputId = 'state_plot_base'),
    fluidRow(
      plotOutput(
        outputId = 'state_plot_ggplot2', height = '650px', width = '100%')
    ),
    br(),
    div(
      class = "footer",
      style='height:50px;background:gray54;margin-top:auto;margin-bottom:auto;
                    text-align:center;',
      HTML(
        '<footer class="footer">
              Copyright &copy; 2024 &nbsp; Website: <a href="https://scalableanalytics.co.ke/"
              target="_blank">Scalable Analytics</a> &nbsp; &nbsp;
              Github Account: <a href="https://github.com/akhapwoyaco"
              target="_blank">akhapwoyaco</a>
              </footer>'
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive state to capture input state data
  us_data <- reactive({
    subset(x = us_avg_tuition, subset = State %in% input$us_state)
  }) 
  # long data for ggplot 2
  us_data_2 <- reactive({
    # pivot so that dates are a column and tuition rates are a column
    us_data() |> 
      pivot_longer(
        cols = !State,  
        names_to = 'year', 
        values_to = 'average_tuition') %>% 
      mutate(year = as.factor(year))
  })
  #
  # Output data per state
  output$state_data <- renderTable({
    us_data()
  }, striped = TRUE, bordered = TRUE, hover = T)
  # Plot output BASE R
  # output$state_plot_base <- renderPlot({
  #   x_val = 1:ncol(us_data())
  #   y_val = t(us_data())
  #   col_names_data = colnames(us_data())
  #   plot(x = x_val, y = y_val,
  #        ylab = 'Average Tuition', xlab = 'Year',
  #        col = 'blue', type = 'b', pch = 19,
  #        col.lab = 'black', cex.lab = 1.2, las = 1,
  #        xaxt = 'n', 
  #        main = paste(
  #          'United States of America:', input$us_state, 'Average Tuition',
  #          sep = ' '))
  #   axis(1, at = x_val, labels = col_names_data)
  #   grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  # })
  # Plot output GGPLOT2
  # ggplot reactive, for later plot and saving
  ggplot_out <- reactive({
    us_data_2() |> 
      ggplot(aes(x = year, y = average_tuition, group = State, color = State)) + 
      geom_line() + 
      geom_point() +  
      labs(y = 'Average Tuition', x = 'Academic Year',
           title = paste(
             'United States of America:', 'Average Tuition',
             sep = ' '),
           subtitle = paste0(input$us_state, collapse = ', '), 
           caption = 'Data Source: https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-04-02/us_avg_tuition.xlsx \n Github: https://github.com/akhapwoyaco/'
      ) + 
      theme_bw() + 
      scale_y_continuous(labels = scales::label_dollar()) +
      theme(
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = 'inside',
        legend.position.inside = c(0.25, 0.85),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'), 
        axis.text.x = element_text(
          vjust = 0.5,
          #hjust = 0.5, 
          angle = 45))
  })
  #
  output$state_plot_ggplot2 <- renderPlot({
    ggplot_out()
  }, res = 100)
  
  # download plot via handler
  output$download_image <- downloadHandler(
    filename = function(){
      # supply state names and paste on filetype
      paste(
        paste0(input$us_state, collapse = '_'),
        '.jpeg', sep = ''
      )
    },
    content = function(file){
      ggsave(
        plot = ggplot_out(), filename = file, 
        width = 30, height = 25, units = "cm", dpi = 450
      )
    }
  )
  #
}

shinyApp(ui, server)