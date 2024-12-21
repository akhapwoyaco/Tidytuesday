library(shiny)
library(tidyverse)
library(ggthemes)
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
    body {
      font-family: 'JetBrains Mono', monospace;
      background-color: black;
      color: white;
      height: 100vh;
      line-height: 2.20rem;
    }
    h2 {
        font-size: 3rem;
        font-weight: 700;
        line-height: calc(2* var(--line-height));
        text-transform: uppercase;
        text-align: center;
        left: 0px;
        right: 0px;
        background-color: black;
        display: flex;
        align-items: center;
        justify-content: center;
        
    }
    .nav, .navbar-nav, .nav-underline {
        display: flex;
        float: none;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
    }
    .container-fluid {
      background-color: black;
      font-size: 2rem;
      font-weight: 700;
    }
      #fluidrow_1 {
        position: relative;
        overflow: hidden;
        transition: all 0.4s ease-in-out;
        border-radius: 5px;
        box-shadow: 2px 1px 25px 5px rgb(166 166 166 / 20%);
        margin: 1rem 0;
        padding: 10px;
        color: #f0f0f0;
        border: 1px solid rgba(17,72,126,0.1);
        background: #11487e url(/themes/kmd/img/bg-tile.jpg) repeat-x 0 top;
        .timendate{
          display: flex;
          align-items: flex-end;
          font-family: $bodyFont;
          .date{
            color: #f0f0f0;
            font-size: 1.3rem;
            font-weight: 600;
          }
          .period{
            color: #f0f0f0;
            font-weight: 400;
            font-size: 1.15rem;
            padding-left: 1rem;
          }
      
        }
      
      
        a{
          color: #f0f0f0;
          font-weight: 500;
          font-size: 14px;
        }
      }
    "))),
  titlePanel("European energy"),
  fluidPage(
    navbarPage(
      title = NULL,
      tabPanel(
        "Energy Types", 
        # tabpanel(
        #   "2017"
        # ),
        fluidRow(
          id = "fluidrow_1",
          plotOutput(
            "energy_types_plot", height = '1000px'
          )
        )
      ),
      tabPanel(
        title = "Country Totals",
        fluidRow(
          id = "fluidrow_1",
          plotOutput(
            "country_totals_plot", height = '1000px'
          )
        )
      )
    )
    # fluidRow(
    #   style = "display: flex; flex-wrap:nowrap; gap: 1ch; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
    # 
    # ),
    
  )
)

server <- function(input, output, session) {
  energy_total = reactive({
    energy_types |> 
      select(-level, -country_name) |> 
      pivot_longer(
        cols = `2016`:`2018`,
        names_to = "year",
        values_to = "total net production" ) |> #get sum of production by country n by year 
      count(
        country, year, wt = `total net production`, 
        name  = 'total', sort = FALSE)
  })
  
  country_total = reactive({
    country_totals |> 
      select(-level, -country_name) |> 
      pivot_longer(
        cols = `2016`:`2018`,
        names_to = "year",
        values_to = "total net production" ) |> #get sum of production by country n by year 
      count(
        country, year, wt = `total net production`, 
        name  = 'total', sort = FALSE)
  })
  
  energy_percent <- reactive({
    energy_types |> 
      select(-level, -country_name) |>
      pivot_longer(
        cols = `2016`:`2018`,
        names_to = "year",
        values_to  = "total net production") |>
      inner_join(
        energy_total(), by = c("country"="country", "year"="year")) |> 
      mutate( 
        percent = (`total net production`/total)*100, 
        percent_sig = case_when(
          type == "Conventional thermal" ~ percent*(-1),
          .default = percent)
      ) |>
      select(-`total net production`,-total) #|> 
    #filter(year == 2017)
  })
  
  country_percent <- reactive({
    country_totals |> 
      select(-level, -country_name) |>
      pivot_longer(
        cols = `2016`:`2018`,
        names_to = "year",
        values_to  = "total net production") |>
      inner_join(
        country_total(), by = c("country"="country", "year"="year")) |> 
      mutate( 
        percent = (`total net production`/total)*100, 
        percent_sig = case_when(
          type == "Total net production" ~ percent*(-1),
          .default = percent)
      ) |>
      select(-`total net production`,-total) #|> 
    #filter(year == 2017)
  })
  
  energy_plot_year <- reactive({
    #print(head(energy_percent()))
    energy_percent() |> 
      ggplot(
        aes(x = reorder(country, -percent_sig), y = percent_sig, fill = type) ) + 
      geom_col(position = "identity", size = 0.1) + 
      scale_y_continuous(labels = scales::unit_format(unit = "%")) +
      scale_fill_brewer(palette = "Dark2") + 
      geom_text(
        aes(label = country), 
        subset(energy_percent(), percent_sig < 0 | percent_sig > 99),
        nudge_y = -3
      ) + 
      facet_wrap(.~year, nrow = 3, scales = 'free') +
      ggthemes::theme_fivethirtyeight() +
      theme(
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"), 
        panel.grid.minor.y = element_line(linetype = "dotted"), 
        legend.position = 'inside',
        legend.position.inside = c(0.23,0.70), 
        legend.background = element_blank(), 
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')
      ) + 
      guides(fill = guide_legend(title = NULL)) + 
      labs(
        title = "Energy Production in Europe", 
        subtitle = "Percentage of Type of Energy to Total Production per Country", 
        caption = "https://github.com/akhapwoyaco"
      )
  })
  
  country_plot_year <- reactive({
    #print(head(energy_percent()))
    country_percent() |> 
      ggplot(
        aes(x = reorder(country, -percent_sig), y = percent_sig, fill = type) ) + 
      geom_col(position = "identity", size = 0.1) + 
      scale_y_continuous(labels = scales::unit_format(unit = "%")) +
      scale_fill_brewer(palette = "Dark2") + 
      geom_text(
        aes(label = country), 
        subset(country_percent(), percent_sig < 0 | percent_sig > 99),
        nudge_y = -3
      ) + 
      facet_wrap(.~year, nrow = 3, scales = 'free') +
      ggthemes::theme_fivethirtyeight() +
      theme(
        axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"), 
        panel.grid.minor.y = element_line(linetype = "dotted"), 
        legend.position = 'inside',
        legend.position.inside = c(0.2,0.65), 
        legend.background = element_blank(), 
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold')
      ) + 
      guides(fill = guide_legend(title = NULL)) + 
      labs(
        title = "Total Energy Production in Europe", 
        subtitle = "Percentage of Type of Energy to Total Production per Country", 
        caption = "https://github.com/akhapwoyaco"
      )
  })
  output$energy_types_plot <- renderPlot({
    energy_plot_year()
  })
  
  output$country_totals_plot  <- renderPlot({
    country_plot_year()
  })
}

shinyApp(ui, server)