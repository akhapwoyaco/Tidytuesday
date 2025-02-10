# tests/testthat/test-map-interactions.R
library(shinytest2)
library(testthat)
library(jsonlite)

# Helper function to create test app
# create_test_app <- function() {
#   testServer(mapModuleServer)#, args = list(
#   #   id = factbook_data
#   # ))
# }

# Test suite for map interactions
test_that("Map interactions trigger correct card updates", {
  # Launch test app
  app <- AppDriver$new(app_dir = ".", name = 'map-interactions')
  
  # Custom JavaScript to get map polygons and simulate clicks
  js_get_polygons <- "
  function polygonGet(){
        const paths = document.querySelectorAll('.leaflet-interactive');
        const polygons = Array.from(paths).filter(p => p.tagName === 'path');
        return polygons.map(p => ({
            id: p.getAttribute('d'),
            bounds: p.getBoundingClientRect()
        }));
  }
    "
  
  # Get all polygon elements
  app$run_js(js_get_polygons)
  polygons <- app$get_js("polygonGet()")
  
  print(1111)
  print(polygons)
  
  # Test multiple random country selections
  for(i in 1:2) {
    print(i)
    # Randomly select a country polygon
    random_polygon <- polygons[[i]]
    print(random_polygon)
    # Simulate click on polygon center
    js_click <- sprintf("
            const polygon = document.querySelector(`.leaflet-interactive[d=\"%s\"]`);
            const bounds = polygon.getBoundingClientRect();
            const centerX = bounds.left + bounds.width / 2;
            const centerY = bounds.top + bounds.height / 2;
            
            const clickEvent = new MouseEvent('click', {
                bubbles: true,
                cancelable: true,
                view: window,
                clientX: centerX,
                clientY: centerY
            });
            
            polygon.dispatchEvent(clickEvent);
        ", random_polygon$id)
    
    app$run_js(js_click)
    
    # Wait for cards to update
    app$wait_for_value(input = "worldMap_shape_click")
    
    # Verify cards are displayed with correct data
    expect_true(app$get_value(output = "countryCards"))
    
    # Check each metric card
    metrics <- list(
      "Country" = " country",
      "Area" = " sq km",
      "Birth Rate" = " per 1,000",
      "Death Rate" = " per 1,000",
      "Infant Mortality" = " per 1,000",
      "Internet Users" = "users",
      "Life Expectancy At Birth" = " years",
      "Maternal Mortality" = " per 100,000",
      "Net Migration" = " per 1,000",
      "Population" = "people",
      "Population Growth" = "%"
    )
    
    for(metric in names(metrics)) {
      # Get card content
      card_value <- app$get_value(
        output = sprintf("cards-%s", gsub(" ", "-", tolower(metric)))
      )
      
      # Verify numeric value and suffix
      expect_match(
        card_value,
        sprintf("^[0-9,.]+%s$", metrics[[metric]]),
        info = sprintf("Testing %s card format", metric)
      )
    }
  }
  
  # Clean up
  app$stop()
})
