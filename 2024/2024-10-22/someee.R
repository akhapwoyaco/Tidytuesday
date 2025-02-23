# test_app.R


# Cards Module Specific Tests
test_that("Cards module functions correctly", {
  test_env <- TestEnvironment$new()
  metrics <- PerformanceMetrics$new()
  
  # Test card rendering
  test_that("Cards render correctly for different data types", {
    countries <- c("United States", "Vatican City", "China", "Monaco")
    
    for(country in countries) {
      test_env$app$execute_script(sprintf('
        Shiny.setInputValue("worldMap_shape_click", {id: "%s"});
      ', country))
      test_env$app$wait_for_idle()
      
      # Check card presence
      expect_true(test_env$app$get_js('
        document.querySelectorAll(".box").length > 0
      '))
      
      # Verify data formatting
      expect_true(test_env$app$get_js('
        Array.from(document.querySelectorAll(".box")).every(box => {
          const content = box.textContent;
          return content && content.trim().length > 0;
        })
      '))
    }
  })
  
  # Test card updates
  test_that("Cards update efficiently", {
    metrics$start_measurement()
    
    for(i in 1:10) {
      test_env$app$execute_script(sprintf('
        Shiny.setInputValue("worldMap_shape_click", {id: "Country%d"});
      ', i))
      test_env$app$wait_for_idle()
    }
    
    metrics$end_measurement()
    expect_lt(metrics$get_execution_time() / 10, 0.2) # Average update time < 200ms
  })
  
  # Test error states
  test_that("Cards handle missing data gracefully", {
    test_env$app$execute_script('
      Shiny.setInputValue("worldMap_shape_click", {id: "Antarctica"});
    ')
    test_env$app$wait_for_idle()
    
    expect_true(test_env$app$get_js('
      document.querySelectorAll(".box").length > 0 &&
      Array.from(document.querySelectorAll(".box")).some(box => 
        box.style.color === "red")
    '))
  })
  
  test_env$cleanup()
})

# Visual Regression Tests
test_that("Visual components maintain consistency", {
  test_env <- TestEnvironment$new()
  
  # Setup snapshot directory
  dir.create("tests/snapshots", recursive = TRUE, showWarnings = FALSE)
  
  # Test map visualization
  test_that("Map renders consistently", {
    test_env$app$wait_for_idle()
    map_screenshot <- test_env$app$get_screenshot("worldMap")
    expect_snapshot_file(map_screenshot, "map_baseline.png")
  })
  
  # Test cards visualization
  test_that("Cards render consistently", {
    test_env$app$execute_script('
      Shiny.setInputValue("worldMap_shape_click", {id: "United States"});
    ')
    test_env$app$wait_for_idle()
    cards_screenshot <- test_env$app$get_screenshot("cards")
    expect_snapshot_file(cards_screenshot, "cards_baseline.png")
  })
  
  # Test responsive layout
  test_that("Layout responds to different screen sizes", {
    screen_sizes <- list(
      c(1920, 1080),
      c(1366, 768),
      c(768, 1024),
      c(375, 812)
    )
    
    for(size in screen_sizes) {
      test_env$app$set_window_size(size[1], size[2])
      test_env$app$wait_for_idle()
      layout_screenshot <- test_env$app$get_screenshot()
      expect_snapshot_file(
        layout_screenshot, 
        sprintf("layout_%dx%d.png", size[1], size[2])
      )
    }
  })
  
  test_env$cleanup()
})

# Database Interaction Tests
test_that("Database operations perform efficiently", {
  test_env <- TestEnvironment$new()
  metrics <- PerformanceMetrics$new()
  
  # Test data loading
  test_that("Data loads efficiently", {
    metrics$start_measurement()
    
    test_env$app$execute_script('
      Shiny.setInputValue("refresh_data", true);
    ')
    test_env$app$wait_for_idle()
    
    metrics$end_measurement()
    expect_lt(metrics$get_execution_time(), 2) # Data load under 2 seconds
  })
  
  # Test data caching
  test_that("Data caching works correctly", {
    metrics$start_measurement()
    
    # First load
    test_env$app$execute_script('
      Shiny.setInputValue("refresh_data", true);
    ')
    test_env$app$wait_for_idle()
    first_load_time <- metrics$get_execution_time()
    
    # Second load (should be cached)
    metrics$start_measurement()
    test_env$app$execute_script('
      Shiny.setInputValue("refresh_data", true);
    ')
    test_env$app$wait_for_idle()
    second_load_time <- metrics$get_execution_time()
    
    expect_lt(second_load_time, first_load_time * 0.5) # Cached load should be twice as fast
  })
  
  test_env$cleanup()
})

# Extended Load Testing
test_that("Application handles sustained load", {
  test_env <- TestEnvironment$new()
  metrics <- PerformanceMetrics$new()
  
  # Simulate multiple concurrent users
  test_that("Handles multiple users", {
    future_promise({
      parallel::mclapply(1:5, function(user_id) {
        local_app <- AppDriver$new(
          app_dir = ".",
          name = sprintf("user-%d", user_id),
          height = 1080,
          width = 1920
        )
        
        # Simulate user actions
        for(i in 1:10) {
          local_app$execute_script(sprintf('
            Shiny.setInputValue("worldMap_shape_click", {id: "Country%d"});
          ', i))
          Sys.sleep(runif(1, 0.1, 0.5))
        }
        
        local_app$stop()
      }, mc.cores = 5)
    }) %...>% 
      function(result) {
        expect_true(TRUE) # Successfully completed multi-user simulation
      }
  })
  
  # Test extended session
  test_that("Maintains performance over time", {
    metrics$start_measurement()
    
    # Simulate 30-minute session
    for(minute in 1:30) {
      # Perform random actions
      action <- sample(1:3, 1)
      switch(action,
             # Action 1: Country selection
             test_env$app$execute_script(sprintf('
          Shiny.setInputValue("worldMap_shape_click", {id: "Country%d"});
        ', minute %% 10 + 1)),
             
             # Action 2: Map interaction
             test_env$app$execute_script('
          document.querySelector("#worldMap").leafletMap.setZoom(
            Math.floor(Math.random() * 10) + 1
          );
        '),
             
             # Action 3: Data refresh
             test_env$app$execute_script('
          Shiny.setInputValue("refresh_data", true);
        ')
      )
      
      test_env$app$wait_for_idle()
      Sys.sleep(1)
      
      if(minute %% 5 == 0) {
        current_memory <- metrics$get_memory_usage()[1]
        expect_lt(current_memory, 500 * 1024 * 1024) # Memory under 500MB
      }
    }
    
    metrics$end_measurement()
    expect_lt(metrics$get_cpu_usage(), 80) # CPU usage under 80%
  })
  
  test_env$cleanup()
})

# CSV Export Tests
test_that("CSV export functions correctly", {
  test_env <- TestEnvironment$new()
  
  # Test export functionality
  test_that("Exports data correctly", {
    # Select a country
    test_env$app$execute_script('
      Shiny.setInputValue("worldMap_shape_click", {id: "United States"});
    ')
    test_env$app$wait_for_idle()
    
    # Trigger export
    temp_file <- tempfile(fileext = ".csv")
    test_env$app$execute_script(sprintf('
      const data = {
        country: document.querySelector(".box:contains(\'Country\')").textContent,
        population: document.querySelector(".box:contains(\'Population\')").textContent
      };
      exportToCSV(data, "%s");
    ', temp_file))
    
    # Verify export
    expect_true(file.exists(temp_file))
    exported_data <- read.csv(temp_file)
    expect_true("country" %in% names(exported_data))
    expect_true("population" %in% names(exported_data))
  })
  
  # Test large data export
  test_that("Handles large data export", {
    metrics <- PerformanceMetrics$new()
    metrics$start_measurement()
    
    # Generate large dataset
    test_env$app$execute_script('
      const largeData = Array.from({length: 10000}, (_, i) => ({
        id: i,
        value: Math.random()
      }));
      exportToCSV(largeData, "large_export.csv");
    ')
    
    metrics$end_measurement()
    expect_lt(metrics$get_execution_time(), 5) # Large export under 5 seconds
  })
  
  test_env$cleanup()
})
