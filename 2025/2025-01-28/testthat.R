# tests/testthat/test-modules.R
library(testthat)
library(shiny)

test_that("geoDataServer handles data loading correctly", {
  testServer(geoDataServer, {
    # Mock data
    testData <- data.frame(
      state = c("NY", "NY", "CA"),
      county = c("Kings", "Queens", "Los Angeles"),
      value = c(10, 20, 30)
    )
    
    # Test file upload
    session$setInputs(data = list(
      datapath = tempfile()
    ))
    saveRDS(testData, session$input$data$datapath)
    
    # Verify data loading
    expect_equal(data(), testData)
    
    # Test state selection
    session$setInputs(state = "NY")
    filtered <- isolate(filtered_data())
    expect_equal(nrow(filtered), 2)
    expect_equal(unique(filtered$state), "NY")
    
    # Test county selection
    session$setInputs(county = "Kings")
    filtered <- isolate(filtered_data())
    expect_equal(nrow(filtered), 1)
    expect_equal(filtered$county, "Kings")
  })
})

test_that("UI elements are created correctly", {
  ui <- geoDataUI("test")
  expect_type(ui, "list")
  expect_true(any(grepl("fileInput", capture.output(print(ui)))))
  expect_true(any(grepl("selectInput", capture.output(print(ui)))))
})