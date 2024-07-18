
save_animal_map <- function(animal_id, dat, output_dir = ".") {
  
  # Subset the data for the specific animal
  animal_data <- subset(dat, individual_id == animal_id)
  
  # Make simple feature geometry for nest location
  sf.nest_animal <- 
    animal_data %>%
    st_as_sf(coords = c('deployment_decimal_longitude', 'deployment_decimal_latitude'), crs = wgs84)
  
  # Ensure the subsetted data is an sf object
  if (!inherits(animal_data, "sf")) {
    animal_data <- st_as_sf(animal_data, coords = c("decimal_longitude", "decimal_latitude"), crs = 4326)
  }
  
  # Create the map for the specific animal
  movementmap <- 
    animal_data %>%
    mapview(zcol = "date",               
            cex = 5, lwd = 0.5, alpha = 1,
            col.regions = viridis::plasma(n = 50),
            na.color = "transparent",
            map.types = 'Esri.WorldImagery',
            crs = wgs84) +
    sf.nest_animal %>% 
    mapview(cex = 5, lwd = 0.5, alpha = 1, pch = 15,
            color = "white",     # line colour for nest
            col.regions = "red")  # fill colour for nest
  
  # Convert the mapview object to a leaflet map
  leaflet_map <- movementmap@map %>%
    addCircles(data = sf.nest_animal,
               lng = ~ first(st_coordinates(sf.nest_animal)[,1]),   #nest long
               lat = ~ first(st_coordinates(sf.nest_animal)[,2]),   # nest lat
               radius = c(150,200),   # radius from nest, in meter                                
               color = "steelblue4",     
               fillColor = "steelblue",
               fillOpacity = 0.4,
               stroke = TRUE,
               opacity = 0.5,
               weight = 2) %>%
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
  
  # Save the leaflet map as an HTML file
  file_name <- paste0(output_dir, animal_id, ".html")
  saveWidget(leaflet_map, file_name, selfcontained = TRUE)
  
  cat("Map saved for animal ID:", animal_id, "at", file_name, "\n")
}
