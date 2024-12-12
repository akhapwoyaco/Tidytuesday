library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(maps)
library(usmap)
us_map_sall = map_data('county') # for plot maps
library(ggplot2) #use ggplot2 to add layer for visualization
#
tuesdata <- tidytuesdayR::tt_load('2018-04-30')
acs2015_county_data <- tuesdata$week5_acs2015_county_data |> 
  select(-CensusId)
# Tract Data
acs2015_county_tract_data <- read_csv("acs2015_census_tract_data.csv") 

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
#'
ui <- dashboardPage(
  skin = 'blue',
  #Dashboard title
  header = dashboardHeader(title = 'County Data Analysis', titleWidth = 250),
  
  #Sidebar layout
  sidebar = dashboardSidebar(
    width = 165,
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = 'data_description',
               icon = icon('gear')),
      menuItem("County Population", tabName = 'county_pop', 
               icon = icon('bar-chart-o')),
      menuItem("County Income", tabName = 'county_income', 
               icon = icon('bar-chart-o')),
      menuItem("County Career", tabName = 'county_career', 
               icon = icon('bar-chart-o')),
      menuItem("County Movement", tabName = 'county_movement', 
               icon = icon('bar-chart-o')),
      menuItem("County Employment", tabName = 'county_employment', 
               icon = icon('bar-chart-o'))) 
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
              height = 150, solidHeader = TRUE, 
              helpText(
                'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-02, and as such we not 
              hold the right to the data: ', 
                tags$a("TidyTuesday data 2018-04-16", 
                       href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-16"),
                tags$br(),
                tags$code("tuesdata <- tidytuesdayR::tt_load('2018-04-16')"),
                tags$br(),
                tags$code("acs2015_county_data <- tuesdata$acs2015_county_data")),
              
              helpText(
                'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
                paste0(
                  'The global mortality data contains ', dim(acs2015_county_data)[1], 'observations, 
            on various countries across the globe over varying durations, with ', 
                  dim(acs2015_county_data)[2], ' features'))),
          box(title = 'Shiny App Guide', status = 'success', width = 12,
              height = 400, solidHeader = TRUE, 
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
                tags$h4("References: "),
                tags$li(
                  tags$a("stackoverflow: customizing how datatables displays missing values in shiny"),
                  href = 'https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny')
              )
              
          )
        )
      ),
      tabItem(
        tabName = 'county_pop',
        fluidPage(
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 2000,
            dataTableOutput(outputId = 'total_population'))
        )
      ),
      
      tabItem(
        tabName = 'county_income',
        fluidPage(
          fluidRow(
            box(
              title = 'Select State', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_income_state', 
                          label = "Select State", multiple = F,
                          choices = unique(acs2015_county_data$State))),
            box(
              title = 'Select County', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_income_county', 
                          label = "Select County", multiple = F,
                          choices = NULL, selected = NULL))
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'county_pop_income'))
        )
      ),
      
      tabItem(
        tabName = 'county_career',
        fluidPage(
          fluidRow(
            box(
              title = 'Select State', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_career_state', 
                          label = "Select State", multiple = F,
                          choices = unique(acs2015_county_data$State))),
            box(
              title = 'Select County', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_career_county', 
                          label = "Select County", multiple = F,
                          choices = NULL, selected = NULL))
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'county_pop_career'))
        )
      ),
      
      tabItem(
        tabName = 'county_movement',
        fluidPage(
          fluidRow(
            box(
              title = 'Select State', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_movement_state', 
                          label = "Select State", multiple = F,
                          choices = unique(acs2015_county_data$State))),
            box(
              title = 'Select County', 
              status = 'primary', width = 5,
              solidHeader = TRUE, height = 120,
              selectInput(inputId = 'in_county_pop_movement_county', 
                          label = "Select County", multiple = F,
                          choices = NULL, selected = NULL))
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'county_pop_movement'))
        )
      ),
      
      tabItem(
        tabName = 'county_employment',
        fluidPage(
          fluidRow(
            column(
              width = 7,
              box(
                title = 'Select State', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_county_pop_employment_state', 
                            label = "Select State", multiple = F,
                            choices = unique(acs2015_county_data$State))),
              box(
                title = 'Select County', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_county_pop_employment_county', 
                            label = "Select County", multiple = F,
                            choices = NULL, selected = NULL))),
            column(
              width = 4,
              fluidRow(
                box(
                  title = 'USA Average Employment per State', 
                  status = 'primary', width = 12,
                  solidHeader = TRUE, height = 400,
                  collapsible = TRUE,
                  # plotOutput
                  plotOutput(outputId = 'county_pop_employment_plot')))
            )
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'county_pop_employment'))
          
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  # County Population
  state_population <- reactive({
    acs2015_county_data |> 
      group_by(State) |>
      summarise(
        Total_Population = sum(TotalPop),
        Men_Population = sum(Men),
        Women_Population = sum(Women)
      ) |> 
      arrange(desc(Total_Population)) |>
      mutate(
        percentage_of_total = round(100*Total_Population/sum(Total_Population), 2),
        ranking = rank(-Total_Population)) 
  })
  
  output$total_population <- renderDT({
    state_population() |> 
      datatable(options = list(
        pageLength = 52,
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        dom = 't', #search and pagination hiding
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE) |> 
      formatStyle(columns = 1:6, border='1px solid #ddd') |>
      formatStyle(
        names(state_population()[5]),
        background = styleColorBar(range(state_population()[,5, drop=F]), 'green'),
        backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', 
        backgroundPosition = 'center')
  })
  
  #' TODO: If there is a way to modularize so as to avoid the repetition below, update later
  #' INCOME
  #' income
  #' Data filtered by country
  acs2015_county_data_state_income <- reactive({
    # Means of income
    income_demo <- c(
      "CensusTract", "State", "County", "Income",# "IncomeErr", 
      "IncomePerCap", #"IncomePerCapErr", 
      "Poverty", "ChildPoverty" 
    )
    acs2015_county_tract_data |> 
      select(any_of(income_demo)) |>
      filter(State == input$in_county_pop_income_state)
  })
  #' county income
  observeEvent(input$in_county_pop_income_state, {
    req(input$in_county_pop_income_state)
    county_req <- acs2015_county_data_state_income() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_county_pop_income_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$county_pop_income <- renderDT(
    acs2015_county_data_state_income() |> 
      filter(County == input$in_county_pop_income_county) |>
      select(-State, -County)  |> 
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        pageLength = 20,
        dom = 'fip', #filter, information on entries and pagination
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE)
  )
  #' 
  
  #' CAREER
  acs2015_county_data_state_career <- reactive({
    # career
    career_demo <- c(
      "CensusTract", "State", "County", "Professional", "Service", 
      "Office", "Construction", "Production" 
    )
    acs2015_county_tract_data |> 
      select(any_of(career_demo)) |>
      filter(State == input$in_county_pop_career_state)
  })
  #' county career
  observeEvent(input$in_county_pop_career_state, {
    req(input$in_county_pop_career_state)
    county_req <- acs2015_county_data_state_career() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_county_pop_career_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$county_pop_career <- renderDT(
    acs2015_county_data_state_career() |> 
      filter(County == input$in_county_pop_career_county) |>
      select(-State, -County) |> 
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        pageLength = 20,
        dom = 'fip', #filter, information on entries and pagination
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE)
  )
  #'
  
  #' Movement
  #' Data filtered by country
  acs2015_county_data_state_movement <- reactive({
    # Means of movement
    transport_demo <- c(
      "CensusTract", "State", "County", "Drive", "Carpool", "Transit", 
      "Walk", "OtherTransp", "WorkAtHome", "MeanCommute"  
    )
    acs2015_county_tract_data |> 
      select(any_of(transport_demo)) |>
      filter(State == input$in_county_pop_movement_state)
  })
  #' county movement
  observeEvent(input$in_county_pop_movement_state, {
    req(input$in_county_pop_movement_state)
    county_req <- acs2015_county_data_state_movement() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_county_pop_movement_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$county_pop_movement <- renderDT(
    acs2015_county_data_state_movement() |> 
      filter(County == input$in_county_pop_movement_county) |>
      select(-State, -County)  |> 
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        pageLength = 20,
        dom = 'fip', #filter, information on entries and pagination
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE)
  )
  
  #'
  #' EMPLOYMENT
  #' Data filtered by country
  acs2015_county_data_state_emp <- reactive({
    # Employment
    employment_demo <- c(
      "CensusTract", "State", "County", "Employed", "PrivateWork", "PublicWork", 
      "SelfEmployed", "FamilyWork", "Unemployment" 
    )
    acs2015_county_tract_data |> 
      select(any_of(employment_demo)) |>
      filter(State == input$in_county_pop_employment_state)
  })
  
  acs2015_county_state_employment <- reactive({
    # Employment
    employment_demo <- c(
      "CensusTract", "State", "County", "Employed", "PrivateWork", "PublicWork", 
      "SelfEmployed", "FamilyWork", "Unemployment" 
    )
    acs2015_emp <- acs2015_county_data |> 
      select(any_of(employment_demo)) |>
      filter(State == input$in_county_pop_employment_state)
    #acs2015_emp$State <- state.abb[match(acs2015_emp$State, state.name)]
    acs2015_emp
  })
  #' county Employment
  observeEvent(input$in_county_pop_employment_state, {
    req(input$in_county_pop_employment_state)
    county_req <- acs2015_county_data_state_emp() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_county_pop_employment_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  
  
  county_pop_employment_data <- reactive({
    acs2015_county_data_state_emp() |> 
      filter(County == input$in_county_pop_employment_county)
  })
  output$county_pop_employment <- renderDT(
    county_pop_employment_data() |>
      select(-State, -County)  |> 
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd', 
        pageLength = 20,
        dom = 'fip', #filter, information on entries and pagination
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE)
  )
  
  output$county_pop_employment_plot <- renderPlot({
    #
    states_state <- us_map_sall[
      us_map_sall$region %in% tolower(input$in_county_pop_employment_state),]
    states_county <- states_state[states_state$subregion %in% tolower(input$in_county_pop_employment_county),]
    #
    ggplot(states_state, aes(long, lat, group = group)) + 
      geom_polygon(fill = 'white', color = 'gray') + 
      geom_polygon(fill = 'red', data = states_county, color = 'white') +
      coord_equal() + theme_void()
    
  }, alt = 'Pr', height = 300, width = 250)
  
}

shinyApp(ui, server)