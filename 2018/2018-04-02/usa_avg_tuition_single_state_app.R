#' 
#' Shiny app to display line plots of each state to note trends
#' in both ggplot and base R
#'
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# #
tuesdata <- tidytuesdayR::tt_load('2018-04-02')
us_avg_tuition <- tuesdata$us_avg_tuition
# #
# library(readr)
# us_avg_tuition <- readxl::read_xlsx("us_avg_tuition.xlsx")
#
ui <- fluidPage(
  titlePanel('Average Tuition USA'),
  mainPanel(
    selectInput(inputId = 'us_state', label = 'State', 
                choices = unique(us_avg_tuition$State)),
    tableOutput(outputId = 'state_data'),
    splitLayout(cellWidths = c(450,450), 
                style = 'border: 1px solid silver:',
                plotOutput(outputId = 'state_plot_base'),
                plotOutput(outputId = 'state_plot_ggplot2')
    ), 
    tags$div(
      id='cite', 'Data Source: ', tags$em('Tidytuesday')
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
    subset(x = us_avg_tuition, subset = State == input$us_state, 
           select = -State)
  }) 
  # long data for ggplot 2
  us_data_2 <- reactive({
    # pivot so that dates are a column and tuition rates are a column
    us_data() |> 
      pivot_longer(
        cols = everything(), 
        names_to = 'year', 
        values_to = 'average_tuition') %>% 
      mutate(year = as.factor(year))
  })
  #
  # Output data per state
  output$state_data <- renderTable({
    us_data()
  }, striped = TRUE, bordered = TRUE)
  # Plot output BASE R
  output$state_plot_base <- renderPlot({
    x_val = 1:ncol(us_data())
    y_val = t(us_data())
    col_names_data = colnames(us_data())
    plot(x = x_val, y = y_val,
         ylab = 'Average Tuition', xlab = 'Year',
         col = 'blue', type = 'b', pch = 19,
         col.lab = 'black', cex.lab = 1.2, las = 1,
         xaxt = 'n', 
         main = paste(
           'United States of America:', input$us_state, 'Average Tuition',
           sep = ' '))
    axis(1, at = x_val, labels = col_names_data)
    grid(nx = NA, ny = NULL, col = "gray", lty = "dashed")
  })
  # Plot output GGPLOT2
  output$state_plot_ggplot2 <- renderPlot({
    #
    us_data_2() |> 
      ggplot(aes(x = year, y = average_tuition, group = 1)) + 
      geom_line() + geom_point(color = 'blue') +  
      labs(y = 'Average Tuition', x = 'Year',
           title = paste(
             'United States of America:', input$us_state, 'Average Tuition',
             sep = ' ')) + 
      theme_bw() + 
      theme(
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(face = 'bold'))
  })
  
}

shinyApp(ui, server)