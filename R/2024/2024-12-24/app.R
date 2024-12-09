#
# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")
library(ggrepel)
library(dplyr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2024-12-24')
global_holidays <- tuesdata$global_holidays
monthly_passengers <- tuesdata$monthly_passengers
#
country_choices = unique(global_holidays$ADM_name)
#
library(shiny)

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
        height: 100vh;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
        text-align: center;
        left: 0px;
        right: 0px;
        background-color: black;
        
      }")) #padding: 6px 12px; padding-top: 0.35em;
  ),
  titlePanel("Holidays"),
  fluidRow(
    column(4, 
           selectInput(
             "country", "Country", choices = country_choices)),
    column(4, 
           uiOutput(outputId = "h_date", inline = T)
    ),
    column(4, 
           uiOutput(outputId = "h_type", inline = T)
    )
  ),
  fluidRow(
    plotOutput('country_plot', height = '800px')
  ),
  fluidRow(
    column(
      width = 6, 
      downloadButton(outputId = "download_image", label = "Download Image")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$country, 
               {
                 req(country_data())
                 output$h_type <- renderUI({
                   radioButtons('holiday_type', "Holiday Type", 
                                inline = T, 
                                choices = c("ALL", unique(country_data()$Type)))
                 })
                 
                 output$h_date <- renderUI({
                   dateRangeInput(
                     "dat_erange", "Date range:",
                     start  = min(country_data()$Date),
                     end    = max(country_data()$Date),
                     # min    = "2001-01-01",
                     # max    = "2012-12-21",
                     format = "yyyy",
                     separator = " - ")
                 })
               }
  )
  country_data <- reactive({
    global_holidays |> 
      filter(ADM_name == input$country) |> 
      mutate(
        y = nchar(Name)*c(-2,2)
      )
  })
  ggplot_out <- reactive({
    req(input$holiday_type)
    if (input$holiday_type == "ALL") {
      plot_data =  country_data()
    } else {
      plot_data =  country_data() |> filter(Type == input$holiday_type)
    }
    # plot_data |> 
    country_data() |>
      ggplot(aes(x = Date, y = y)) + 
      # geom_linerange(
      #   aes(x = Date, y = y, ymin = 0,
      #       # color = Type,
      #       ymax = (\(x) ifelse(x > 0, x-2,x+2))(y) )
      # ) +
      geom_linerange(
        data = plot_data,
        aes(x = Date, y = y, ymin = 0,
            color = Type,
            ymax = (\(x) ifelse(x > 0, x-2,x+2))(y) )
      ) +
      geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
      theme_classic() +
      ggrepel::geom_text_repel(
        data = plot_data,
        aes(x = Date, y = y, 
            label = Name |> trimws(), 
            color = Type)) +
      scale_color_brewer(palette = 'Dark2') +
      theme(
        axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        # 
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        legend.position = "top"
      )
    
  })
  #
  output$country_plot <- renderPlot({
    ggplot_out()
  })
  # download plot via handler
  output$download_image <- downloadHandler(
    filename = function(){
      # supply state names and paste on filetype
      paste(
        paste(input$country, input$holiday_type, sep = '_'),
        '.jpeg', sep = ''
      )
    },
    content = function(file){
      ggsave(
        plot = ggplot_out(), filename = file, 
        width = 45, height = 30, units = "cm", dpi = 750
      )
    }
  )
}

shinyApp(ui, server)