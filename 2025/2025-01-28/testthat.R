library(shinytest2)
library(tidyverse)

app_dir = "."
# Test file: tests/testthat/test-outputs.R
county_state_data = readRDS("data/county_state_data.rds")
unique_states = unique(county_state_data$state) |> sort()
###########################################################################
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
        `geodata-state` = state_name,
        `geodata-county` = county_i
      )
      #
      # table not null
      expect_true({
        app$get_js("
      document.querySelector('#averages_table table') !== null
    ")
      })
      # presence of filte
      expect_true(
        app$get_js(
          "document.querySelector('#averages_table').getElementsByClassName('dataTables_filter').length == 0;"
        )
      )
      # presence of pagination
      expect_true(
        app$get_js(
          "document.querySelector('#averages_table').getElementsByClassName('dataTables_paginate').length == 0;"
        )
      )
      
      # presence of sorting
      expect_true(
        app$get_js(
          "document.querySelector('#averages_table').getElementsByClassName('sorting').length > 0;"
        )
      )
    }
  }
  
})
#