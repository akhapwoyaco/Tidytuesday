library(shiny)
library(purrr)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(maps)
library(usmap)
library(ggplot2) #use ggplot2 to add layer for visualization

us_map_sall = map_data('county') # for plot maps
#

#
# acs2015_county_data <- read_csv("week5_acs2015_county_data.csv", 
#                                 show_col_types = FALSE) |>
#   select(-CensusId)
# Or read in the data manually
tuesdata <- tidytuesdayR::tt_load('2018-04-30')
acs2015_county_data <- tuesdata$week5_acs2015_county_data |> 
  select(-CensusId)
#
#
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
#' MENU ITEMS
menu_item_vars <- tibble::tribble(
  ~text, ~tab_name, ~i_con,
  "Introduction", 'data_description', 'gear', 
  "County Population", 'state_total_gender_population', 'table',
  "County Income", 'state_income', 'table', 
  "County Career", 'state_career', 'table',
  "County Movement", 'state_movement', 'table',
  "County Employment", 'state_employment', 'table')

tab_naming_function <- function(text = text, tab_name = tab_name, i_con = i_con){
  menuItem(text = text, tabName = tab_name, icon = icon(i_con))
}
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
      id = "tabs", # Using functions in Shiny
      pmap(menu_item_vars, tab_naming_function)
    ) 
  ),
  
  #Tabs 
  
  body = dashboardBody(
    
    tags$head(
      tags$style(HTML(
        '.main-header .logo {font-weight: bold;} .box-body {font-size: 11px}'
      ))),
    #' 
    tabItems(
      
      # County Population TabItem
      tabItem(
        tabName = 'data_description',
        fluidPage(
          # Explain the source of data
          box(title = 'Data Source', status = 'success', width = 12,
              height = 200, solidHeader = TRUE, 
              helpText(
                'The data was acquired from the TidyTuesday Github account: 
              TidyTuesday data 2018-04-30, and as such we not 
              hold the right to the data: ', 
                tags$a("TidyTuesday data 2018-04-30", 
                       href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-04-16"),
                tags$br(),
                tags$code("tuesdata <- tidytuesdayR::tt_load('2018-04-30')"),
                tags$br(),
                tags$code("acs2015_county_data <- tuesdata$week5_acs2015_county_data")),
              
              helpText(
                'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
                paste0(
                  'The acs 2015 county data contains ', dim(acs2015_county_data)[1], ' observations, 
            on states within the United State of America for the year 2015, with ', 
                  dim(acs2015_county_data)[2], ' features'))),
          box(title = 'Shiny App Guide', status = 'success', width = 12,
              height = 400, solidHeader = TRUE, 
              helpText(
                # include the html for lista and bold text
                tags$h4('Introduction:'),
                tags$li(
                  'Source of data is explained, along with period the data covers.'),
                tags$h4("County Population: "),
                tags$li(
                  'Provides a table on the total population per state and associated 
                proportion to USA national population, the population distribution by 
                gender, and the rank on population size.'),
                tags$h4("Interesting Stuffs: "),
                tags$li(
                  'Bar Plot Coloring per Column.'),
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
        #' table on the ranked total population along with by gender distribution
        tabName = 'state_total_gender_population',
        fluidPage(
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 2000,
            dataTableOutput(outputId = 'total_gender_population'))
        )
      ), 
      
      #'
      #' The state counties income data
      tabItem(
        tabName = 'state_income',
        fluidPage(
          fluidRow(
            column(
              width = 7,
              box(
                title = 'Select State', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 160,
                selectInput(inputId = 'in_state_income_state', 
                            label = "Select State", multiple = F,
                            choices = unique(acs2015_county_data$State))),
              box(
                title = 'Select County', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 160,
                selectInput(inputId = 'in_state_income_county', 
                            label = "Select County", multiple = F,
                            choices = NULL, selected = NULL))),
            column(
              width = 4,
              fluidRow(
                box(
                  title = 'USA Average Employment per State', 
                  status = 'primary', width = 12,
                  solidHeader = TRUE, height = 400,
                  # plotOutput
                  plotOutput(outputId = 'out_state_county_income_plot')))
            )
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'out_state_county_income'))
        )
      ),
      
      #' 
      #' CAREER
      #' 
      tabItem(
        tabName = 'state_career',
        fluidPage(
          fluidRow(
            column(
              width = 7,
              box(
                title = 'Select State',
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_career_state',
                            label = "Select State", multiple = F,
                            choices = unique(acs2015_county_data$State))),
              box(
                title = 'Select County',
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_career_county',
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
                  plotOutput(outputId = 'out_state_county_career_plot')))
            )
          ),
          box(
            title = 'USA Total Population per State',
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'out_state_county_career'))
        )
      ),
      
      tabItem(
        tabName = 'state_movement',
        fluidPage(
          fluidRow(
            column(
              width = 7,
              box(
                title = 'Select State', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_movement_state', 
                            label = "Select State", multiple = F,
                            choices = unique(acs2015_county_data$State))),
              box(
                title = 'Select County', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_movement_county', 
                            label = "Select County", multiple = F,
                            choices = NULL, selected = NULL))),
            column(
              width = 4,
              fluidRow(
                box(
                  title = 'USA Movement per State', 
                  status = 'primary', width = 12,
                  solidHeader = TRUE, height = 400,
                  # plotOutput
                  plotOutput(outputId = 'out_state_county_movement_plot')))
            )
          )
        ),
        box(
          title = 'USA Total Population per State', 
          status = 'primary', width = 10,
          solidHeader = TRUE, height = 800,
          dataTableOutput(outputId = 'out_state_county_movement')
        )),
      
      tabItem(
        tabName = 'state_employment',
        fluidPage(
          fluidRow(
            column(
              width = 7,
              box(
                title = 'Select State', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_employment_state', 
                            label = "Select State", multiple = F,
                            choices = unique(acs2015_county_data$State))),
              box(
                title = 'Select County', 
                status = 'primary', width = 12,
                solidHeader = TRUE, height = 180,
                selectInput(inputId = 'in_state_employment_county', 
                            label = "Select County", multiple = F,
                            choices = NULL, selected = NULL))),
            column(
              width = 4,
              fluidRow(
                box(
                  title = 'USA Average Employment per State', 
                  status = 'primary', width = 12,
                  solidHeader = TRUE, height = 400,
                  # plotOutput
                  plotOutput(outputId = 'out_state_county_employment_plot')))
            )
          ),
          box(
            title = 'USA Total Population per State', 
            status = 'primary', width = 10,
            solidHeader = TRUE, height = 800,
            dataTableOutput(outputId = 'out_state_county_employment'))
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
        total_gender_population = sum(TotalPop),
        Men_Population = sum(Men),
        Women_Population = sum(Women)
      ) |>
      arrange(desc(total_gender_population)) |>
      mutate(
        percentage_of_total = round(100*total_gender_population/sum(total_gender_population), 2),
        ranking = rank(-total_gender_population))
  })
  
  output$total_gender_population <- renderDT({
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
        background = styleColorBar(range(state_population()[,5, drop=F]), 'lightgreen'),
        backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center')
  })
  
  #' 
  #' INCOME
  #' Data filtered by county
  acs2015_county_data_state_income <- reactive({
    # Means of income
    income_demo <- c(
      "CensusTract", "State", "County", "Income", "IncomePerCap",
      "Poverty", "ChildPoverty"
    )
    acs2015_county_data |>
      select(any_of(income_demo)) |>
      filter(State == input$in_state_income_state)
  })
  #' county income
  observeEvent(input$in_state_income_state, {
    req(input$in_state_income_state)
    county_req <- acs2015_county_data_state_income() |>
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_state_income_county',
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$out_state_county_income <- renderDT(
    acs2015_county_data_state_income() |>
      #filter(County == input$in_county_income_county) |>
      select(-State)  |>
      datatable(options = list(
        rowCallback = JS(rowCallback),
        border = '1px solid #ddd',
        pageLength = 20,
        dom = 'fip', #filter, information on entries and pagination
        scrollX = TRUE # scroll across the columns when view reduced
      ),width = '100%', height = '100%', #table remains in container,
      rownames = FALSE)
  )
  
  output$out_state_county_income_plot <- renderPlot({
    #
    states_state <- us_map_sall[
      us_map_sall$region %in% tolower(input$in_state_income_state),]
    states_county <- states_state[
      states_state$subregion %in% tolower(input$in_state_income_county),]
    #
    ggplot(states_state, aes(long, lat, group = group)) + 
      geom_polygon(fill = 'white', color = 'gray') + 
      geom_polygon(fill = 'red', data = states_county, color = 'white') +
      coord_equal() + theme_void()
  }, alt = 'Pr', height = 300, width = 250)
  
  
  #' 
  #' CAREER
  #' 
  acs2015_county_data_state_career <- reactive({
    #' career
    career_demo <- c(
      "CensusTract", "State", "County", "Professional", "Service",
      "Office", "Construction", "Production"
    )
    acs2015_county_data |>
      select(any_of(career_demo)) |>
      filter(State == input$in_state_career_state)
  })
  #' county career
  observeEvent(input$in_state_career_state, {
    req(input$in_state_career_state)
    county_req <- acs2015_county_data_state_career() |>
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_state_career_county',
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$out_state_county_career <- renderDT(
    acs2015_county_data_state_career() |>
      select(-State) |>
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
  output$out_state_county_career_plot <- renderPlot({
    #
    states_state <- us_map_sall[
      us_map_sall$region %in% tolower(input$in_state_career_state),]
    states_county <- states_state[
      states_state$subregion %in% tolower(input$in_state_career_county),]
    #
    ggplot(states_state, aes(long, lat, group = group)) +
      geom_polygon(fill = 'white', color = 'gray') +
      geom_polygon(fill = 'red', data = states_county, color = 'white') +
      coord_equal() + theme_void()
  }, alt = 'Pr', height = 300, width = 250)
  
  #' 
  #' Movement
  #' 
  acs2015_county_data_state_movement <- reactive({
    # Means of movement
    transport_demo <- c(
      "CensusTract", "State", "County", "Drive", "Carpool", "Transit", 
      "Walk", "OtherTransp", "WorkAtHome", "MeanCommute"  
    )
    acs2015_county_data |> 
      select(any_of(transport_demo)) |>
      filter(State == input$in_state_movement_state)
  })
  #' county movement
  observeEvent(input$in_state_movement_state, {
    req(input$in_state_movement_state)
    county_req <- acs2015_county_data_state_movement() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_state_movement_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  output$out_state_county_movement <- renderDT(
    acs2015_county_data_state_movement() |> 
      #filter(County == input$in_state_total_gender_population_movement_county) |>
      select(-State)  |> 
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
  output$out_state_county_movement_plot <- renderPlot({
    #
    states_state <- us_map_sall[
      us_map_sall$region %in% tolower(input$in_state_movement_state),]
    states_county <- states_state[
      states_state$subregion %in% tolower(input$in_state_movement_county),]
    #
    ggplot(states_state, aes(long, lat, group = group)) + 
      geom_polygon(fill = 'white', color = 'gray') + 
      geom_polygon(fill = 'red', data = states_county, color = 'white') +
      coord_equal() + theme_void()
  }, alt = 'Pr', height = 300, width = 250)
  
  #'
  #' Employment
  #' 
  acs2015_county_data_state_emp <- reactive({
    # Employment
    employment_demo <- c(
      "CensusTract", "State", "County", "Employed", "PrivateWork", "PublicWork", 
      "SelfEmployed", "FamilyWork", "Unemployment" 
    )
    acs2015_county_data |> 
      select(any_of(employment_demo)) |>
      filter(State == input$in_state_employment_state)
  })
  
  #' county Employment
  observeEvent(input$in_state_employment_state, {
    req(input$in_state_employment_state)
    county_req <- acs2015_county_data_state_emp() |> 
      distinct(County) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'in_state_employment_county', 
      choices = c(county_req), selected = county_req[1], server = T)
  })
  
  # Employment Table
  output$out_state_county_employment <- renderDT(
    acs2015_county_data_state_emp() |>
      select(-State)  |> 
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
  #'   
  output$out_state_county_employment_plot <- renderPlot({
    states_state <- us_map_sall[
      us_map_sall$region %in% tolower(input$in_state_employment_state),]
    states_county <- states_state[
      states_state$subregion %in% tolower(input$in_state_employment_county),]
    #
    ggplot(states_state, aes(long, lat, group = group)) + 
      geom_polygon(fill = 'white', color = 'gray') + 
      geom_polygon(fill = 'red', data = states_county, color = 'white') +
      coord_equal() + theme_void()
  }, alt = 'Pr', height = 300, width = 250)
  #'
  #' 
  
}

shinyApp(ui, server)