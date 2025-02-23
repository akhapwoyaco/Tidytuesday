library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(ggrepel)
#
canada <- read_csv("canada_births_1991_2022.csv")
#
ui <- fluidPage(
  includeCSS('main.css'),
  titlePanel(title = "Canada Births"),
  plotOutput('time_series', height = 650),
  fluidRow(
    column(3, downloadButton(outputId = "downloadPlot", label = "Downloald Plot")),
    column(3, radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                           inline = TRUE),
           br(),
           downloadButton('downloadReport'),
           ),
    column(3, downloadButton(outputId = "downloadData", label = "Downloald Data")),
    column(3, 
           actionButton(inputId='fiver', label="Fiver R Shiny Services", 
                        icon = icon("th"), 
                        onclick ="window.open('https://www.fiverr.com/s/jj5dYam', '_blank')"))
  )
)
#
server <- function(input, output, session) {
  data_births = reactive({
    canada |>
      mutate(
        date = lubridate::ym(paste(year, month))
      )
  })
  #
  births_lowest_years = reactive({
    req(data_births())
    data_births() |>
      group_by(year) |>
      summarise(
        max = max(births),
        max_date = date[which.max(births)],
        min = min(births),
        min_date = date[which.min(births)],
      )
  })
  #
  births_plot <- reactive({
    req(data_births())
    req(births_lowest_years())
    #
    min_date_x = min(data_births()$date) %m-% months(2) 
    max_date_x = max(data_births()$date) %m+% months(2)
    #
    data_births() |> 
      ggplot() +
      geom_line(
        aes(x = date, y = births)
      ) +
      geom_point(
        data = births_lowest_years(),
        aes(x = max_date, y = max), color = 'red', size = 3 ) +
      geom_point(
        data = births_lowest_years(),
        aes(x = min_date, y = min), color = 'blue', size = 3 ) +
      geom_text_repel(
        data = births_lowest_years(),
        aes(x = max_date, y = max, 
            label = paste(format(max_date, "%b"), "\n", max)), 
        colour = 'red', force = 5,
        box.padding = 0.8, point.padding = 0.5, 
        segment.color = 'red', segment.size = 0.3, min.segment.length = 0.1
      ) +
      geom_text_repel(
        data = births_lowest_years(),
        aes(x = min_date, y = min, 
            label = paste(format(min_date,"%b"), "\n", min)), 
        colour = 'blue', force = 5,
        box.padding = 0.8, point.padding = 0.5, 
        segment.color = 'blue', segment.size = 0.3, min.segment.length = 0.1
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year", 
                   expand = c(0,0), limits = c(min_date_x, max_date_x)) +
      theme_light() +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linetype = 6, linewidth = 0.5)
      )
  })
  #
  output$time_series <- renderPlot({
    births_plot()
  })
  #
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Canada", ".jpeg")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      ggsave(filename = file, plot =  births_plot(), device = "jpeg", 
             width = 40, height = 25, units = 'cm', dpi = 750)
    }
  )
  #
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("Canada", ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data_births(), file, row.names = F)
    }
  )
  #
  # Download Report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(
        paste('Canada', "births", sep = "_"),
        sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(toc = TRUE, toc_depth = 4), 
        HTML = html_document(toc = TRUE, toc_depth = 4), 
        Word = word_document(toc = TRUE, toc_depth = 4)
      )
      )
      file.rename(out, file)
    }
  )
}

shinyApp(ui, server)