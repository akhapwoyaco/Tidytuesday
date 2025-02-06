library(shinytest2)
library(tidyverse)

app_dir = "."
# Test file: tests/testthat/test-outputs.R
county_state_data = readRDS("data/county_state_data.rds")
# sample a few data points
state_county_nrow = nrow(county_state_data)
test_indices = sample(
  x = 1:state_county_nrow, 
  size = state_county_nrow*0.5/100)
county_state_data = county_state_data[test_indices, ]
#
unique_states = unique(county_state_data$state) |> sort()
#
# Setting up tests for modules in shiny
test_that("Tables are rendered correctly", {
  app <- AppDriver$new(app_dir = app_dir, name = "table-test")
  #
  for (state_name in unique_states){
    data_county_s = county_state_data |> 
      filter(state %in% state_name) |> pull(county) |> unique()# |> head(2)
    for (county_i in data_county_s){
      print(state_name)
      print(county_i)
      app$set_inputs(
        `the_geodata_ui-state` = state_name,
        `the_geodata_ui-county` = county_i
      )
      #
      # table not null
      expect_true({
        app$get_js(
        "$('#c_averages_table-averages_table').length > 0"
      )
      })
      
      
      
      #
      # presence of filter
      # # "document.querySelector('#averages_table').getElementsByClassName('dataTables_filter').length == 0;"
      # expect_true(
      #   app$get_js(
      #     "$('#c_averages_table-averages_table')"
      #   )
      # )
      # # presence of pagination
      # # "document.querySelector('#averages_table').getElementsByClassName('dataTables_paginate').length == 0;"
      # expect_true(
      #   app$get_js(
      #     
      #   )
      # )
      # 
      # # presence of sorting
      # # "document.querySelector('#averages_table').getElementsByClassName('sorting').length > 0;"
      # expect_true(
      #   app$get_js(
      #     
      #   )
      # )
    }
  }
  
})
#