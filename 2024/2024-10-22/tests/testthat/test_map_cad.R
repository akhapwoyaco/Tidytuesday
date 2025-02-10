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
  app <- AppDriver$new(app_dir = "../../")
  
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

# # Test map rendering and data binding
# test_that("Map renders with correct data binding", {
#   app <- AppDriver$new("../../")
#   
#   # Test map initialization
#   expect_true(app$get_value(output = "worldMap"))
#   
#   # Verify leaflet map properties
#   js_map_properties <- "
#         const map = document.querySelector('#worldMap');
#         return {
#             hasLegend: !!map.querySelector('.leaflet-control-legend'),
#             hasPolygons: !!map.querySelectorAll('.leaflet-interactive').length,
#             hasTiles: !!map.querySelector('.leaflet-tile-container').children.length
#         };
#     "
#   
#   map_properties <- app$run_js(js_map_properties)
#   expect_true(map_properties$hasLegend)
#   expect_true(map_properties$hasPolygons)
#   expect_true(map_properties$hasTiles)
#   
#   app$stop()
# })
# 
# # Test polygon highlighting
# test_that("Polygons highlight on hover", {
#   app <- AppDriver$new("../../")
#   
#   # Test hover effect
#   js_test_hover <- "
#         const polygon = document.querySelector('.leaflet-interactive');
#         const originalStyle = window.getComputedStyle(polygon);
#         
#         // Simulate hover
#         const hoverEvent = new MouseEvent('mouseover', {
#             bubbles: true,
#             cancelable: true,
#             view: window
#         });
#         polygon.dispatchEvent(hoverEvent);
#         
#         const hoveredStyle = window.getComputedStyle(polygon);
#         
#         // Simulate hover end
#         const leaveEvent = new MouseEvent('mouseout', {
#             bubbles: true,
#             cancelable: true,
#             view: window
#         });
#         polygon.dispatchEvent(leaveEvent);
#         
#         return {
#             original: {
#                 fillOpacity: originalStyle.fillOpacity,
#                 weight: originalStyle.strokeWidth
#             },
#             hovered: {
#                 fillOpacity: hoveredStyle.fillOpacity,
#                 weight: hoveredStyle.strokeWidth
#             }
#         };
#     "
#   
#   styles <- app$run_js(js_test_hover)
#   expect_true(as.numeric(styles$hovered$fillOpacity) > 
#                 as.numeric(styles$original$fillOpacity))
#   expect_true(as.numeric(styles$hovered$weight) > 
#                 as.numeric(styles$original$weight))
#   
#   app$stop()
# })
# 
# # Test popup content
# test_that("Popups display correct information", {
#   app <- AppDriver$new("../../")
#   
#   # Test popup content
#   js_test_popup <- "
#         const polygon = document.querySelector('.leaflet-interactive');
#         const event = new MouseEvent('click', {
#             bubbles: true,
#             cancelable: true,
#             view: window
#         });
#         polygon.dispatchEvent(event);
#         
#         // Wait for popup
#         return new Promise(resolve => {
#             setTimeout(() => {
#                 const popup = document.querySelector('.leaflet-popup-content');
#                 resolve(popup ? popup.innerHTML : null);
#             }, 500);
#         });
#     "
#   
#   popup_content <- app$run_js(js_test_popup)
#   expect_match(popup_content, "<strong>.+</strong>")
#   expect_match(popup_content, "Population: [0-9,]+")
#   
#   app$stop()
# })
# 
# # Test card value formatting
# test_that("Card values are properly formatted", {
#   app <- AppDriver$new("../../")
#   
#   # Click a country
#   js_click_first_country <- "
#         const polygon = document.querySelector('.leaflet-interactive');
#         const event = new MouseEvent('click', {
#             bubbles: true,
#             cancelable: true,
#             view: window
#         });
#         polygon.dispatchEvent(event);
#     "
#   
#   app$run_js(js_click_first_country)
#   app$wait_for_value(input = "worldMap_shape_click")
#   
#   # Test numeric formatting
#   js_get_card_values <- "
#         const cards = document.querySelectorAll('.metric-card');
#         return Array.from(cards).map(card => ({
#             title: card.querySelector('.box-title').textContent,
#             value: card.querySelector('.box-body').textContent.trim()
#         }));
#     "
#   
#   card_values <- app$run_js(js_get_card_values)
#   
#   for(card in card_values) {
#     # Check number formatting
#     expect_match(
#       card$value,
#       "^[0-9,.]+[^0-9]*$",
#       info = sprintf("Testing %s value format", card$title)
#     )
#     
#     # Verify suffix
#     suffix <- switch(
#       card$title,
#       "Country" = " country",
#       "Area" = " sq km",
#       "Birth Rate" = " per 1,000",
#       "Death Rate" = " per 1,000",
#       "Infant Mortality" = " per 1,000",
#       "Internet Users" = "users",
#       "Life Expectancy At Birth" = " years",
#       "Maternal Mortality" = " per 100,000",
#       "Net Migration" = " per 1,000",
#       "Population" = "people",
#       "Population Growth" = "%"
#     )
#     
#     if(suffix != "") {
#       expect_match(
#         card$value,
#         sprintf("%s$", suffix),
#         info = sprintf("Testing %s suffix", card$title)
#       )
#     }
#   }
#   
#   app$stop()
# })