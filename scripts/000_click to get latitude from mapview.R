library(shiny)
library(mapview)
library(viridis)
library(sf)
library(leaflet)

# Define UI for application
ui <- fluidPage(
  titlePanel("Movement Map with Mouse Position and Click Coordinates"),
  leafletOutput("movementmap"),
  verbatimTextOutput("coords")
)

# Define server logic required to draw the map and capture mouse position and clicks
server <- function(input, output, session) {
  
  output$movementmap <- renderLeaflet({
    movementmap <- 
      sf %>%
      mapview(zcol = "date",               
              cex = 5, lwd = 0.5, alpha = 1,
              col.regions = viridis::plasma(n = 50),
              na.color = "transparent",
              map.types = 'Esri.WorldImagery',
              crs = wgs84) +
      sf.nest %>% 
      mapview(cex = 5, lwd = 0.5, alpha = 1, pch = 15,
              color = "white",     # line colour for nest
              col.regions = "red")  # fill colour for nest
    
    movementmap@map %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.on('mousemove', function(e) {
            var lat = e.latlng.lat.toFixed(5);
            var lng = e.latlng.lng.toFixed(5);
            Shiny.onInputChange('mousemove_coords', {lat: lat, lng: lng});
          });
          this.on('click', function(e) {
            var lat = e.latlng.lat.toFixed(5);
            var lng = e.latlng.lng.toFixed(5);
            Shiny.onInputChange('click_coords', {lat: lat, lng: lng});
          });
        }
      ")
  })
  
  observe({
    coords <- input$mousemove_coords
    if (!is.null(coords)) {
      output$coords <- renderPrint({
        paste("Latitude:", coords$lat, "Longitude:", coords$lng)
      })
    }
  })
  
  observeEvent(input$click_coords, {
    click_coords <- input$click_coords
    if (!is.null(click_coords)) {
      cat("Clicked at Latitude:", click_coords$lat, "Longitude:", click_coords$lng, "\n")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
