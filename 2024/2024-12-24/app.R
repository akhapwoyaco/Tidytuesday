#
# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")
library(ggrepel)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
#
#tuesdata <- tidytuesdayR::tt_load('2024-12-24')
global_holidays <- read_csv("global_holidays.csv")#tuesdata$global_holidays
monthly_passengers <- read_csv("monthly_passengers.csv")#tuesdata$monthly_passengers
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
    plotOutput('country_plot', height = '800px'),
    br(),
    plotOutput('box_country_plot', height = '800px')
  ),
  br(),
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
                   sliderInput(
                     "dat_erange", "Date range:",
                     value = c(year(min(country_data()$Date)), year(max(country_data()$Date))),
                     min = year(min(country_data()$Date)),
                     max = year(max(country_data()$Date))#,
                     #step = 1
                     # min    = "2001-01-01",
                     # max    = "2012-12-21",
                     # format = "yyyy",
                     # separator = " - "
                   )
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
  plot_data_filtered <- reactive({
    req(input$holiday_type)
    
    if (input$holiday_type == "ALL") {
      plot_data =  country_data() |>
        filter(Date >= paste(input$dat_erange[1], '-01-01', sep='') & Date <= paste(input$dat_erange[2], sep='','-12-31'))
    } else {
      plot_data =  country_data() |> filter(Type == input$holiday_type) |> 
        filter(Date >= paste(input$dat_erange[1], '-01-01', sep='') & Date <= paste(input$dat_erange[2], sep='','-12-31'))
    }
  })
  ggplot_out <- reactive({
    plot_data_filtered() |>
     ggplot(aes(x = Date, y = y)) +
      geom_linerange(
        aes(x = Date, y = y, ymin = 0,
            color = Type,
            ymax = (\(x) ifelse(x > 0, x-2,x+2))(y) )
      ) +
      geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
      theme_classic() +
      ggrepel::geom_text_repel(
        aes(x = Date, y = y, 
            label = Name |> trimws(), 
            color = Type)) +
      scale_color_brewer(palette = 'Dark2') +
      scale_x_date(date_labels = "%m", date_breaks="6 month", expand=c(0,0) ) +
      facet_grid( ~ year(Date), space="free_x", scales="free_x", switch="x") +
      theme(
        axis.line = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        #
        plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
        legend.position = "top",
        # 
        strip.placement = "outside",
        # panel.border = element_rect(colour="grey70", linewidth = 0),
        # panel.spacing=unit(0,"cm"),
        # strip.text.x = element_text(size = rel(0.8)),
        # panel.grid.minor.x = element_blank(),
        # panel.grid.major.x = element_blank(),
        # axis.line.y = element_blank(),
        # panel.grid.minor = element_blank(),
        # plot.title = element_text(face = "bold", hjust = 0.5),
        # legend.text = element_text(face = "bold", size = rel(1.2)),
        # 
        # axis.text.x = element_text(face = "bold", size = rel(1), angle = 0),
        # axis.text.y = element_text(face = "bold", size = rel(1))
      )
    
  })
  #
  output$country_plot <- renderPlot({
    ggplot_out()
  })
  #
  boxplot_data <- reactive({
    req(country_data())
    ISO_3 = unique(country_data()$ISO3)
    #
    monthly_passengers |>
      filter(ISO3 == ISO_3) |> 
      mutate(
        Date = ym(paste(Year, Month))
      ) |> 
      pivot_longer(
        cols = Total:Total_OS,
        values_to = 'values',
        names_to = 'names'
      )
    #
  })
  #
  
  output$box_country_plot <- renderPlot({
    boxplot_data() |>
      ggplot() +
      geom_line(
        aes(x = Date, y = values,
            color = names)
      ) +
      #geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
      theme_classic() #+
      # ggrepel::geom_text_repel(
      #   aes(x = Date, y = y, 
      #       label = Name |> trimws(), 
      #       color = Type)) +
      # scale_color_brewer(palette = 'Dark2') +
      # scale_x_date(date_labels = "%m", date_breaks="6 month", expand=c(0,0) ) +
      # facet_grid( ~ year(Date), space="free_x", scales="free_x", switch="x") +
      # theme(
      #   axis.line = element_blank(), 
      #   axis.text.y = element_blank(), 
      #   axis.ticks = element_blank(),
      #   axis.title = element_blank(),
      #   legend.title = element_blank(),
      #   #
      #   plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
      #   legend.position = "top",
      #   # 
      #   strip.placement = "outside",
      #   # panel.border = element_rect(colour="grey70", linewidth = 0),
      #   # panel.spacing=unit(0,"cm"),
      #   # strip.text.x = element_text(size = rel(0.8)),
      #   # panel.grid.minor.x = element_blank(),
      #   # panel.grid.major.x = element_blank(),
      #   # axis.line.y = element_blank(),
      #   # panel.grid.minor = element_blank(),
      #   # plot.title = element_text(face = "bold", hjust = 0.5),
      #   # legend.text = element_text(face = "bold", size = rel(1.2)),
      #   # 
      #   # axis.text.x = element_text(face = "bold", size = rel(1), angle = 0),
      #   # axis.text.y = element_text(face = "bold", size = rel(1))
      # )
  })
  # download plot via handler
  output$download_image <- downloadHandler(
    filename = function(){
      # supply state names and paste on filetype
      paste(
        paste(input$country, input$holiday_type, input$dat_erange[1],input$dat_erange[2], sep = '_'),
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