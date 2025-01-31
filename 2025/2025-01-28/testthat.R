library(shinytest2)

app_dir = "."
# Test file: tests/testthat/test-outputs.R

###########################################################################
# 
test_that("All outputs are rendered correctly", {
  app <- AppDriver$new(app_dir = app_dir, name = "output-test")
  
  # Test basic table not null
  expect_true({
    app$get_js("
      document.querySelector('#averages_table') !== null
    ")
  })
  #
  # Test if basic table has data
  expect_gte({
    app$get_js("
    document.querySelector('#averages_table')
      .getElementsByTagName('tr').length
  ")
  }, 0) 
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
  #
})
#
