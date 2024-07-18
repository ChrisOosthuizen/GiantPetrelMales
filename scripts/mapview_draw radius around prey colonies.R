
#-----------------------------
# Specifying locations
#-----------------------------
# Get prey location data: 
prey = read.csv('./data/lat_lon_prey_colonies_simple.csv')
head(prey)

# make labels
prey$site_spp = paste0(prey$site,"; ", prey$spp)

dim(prey)

#----------------------------
# Plot and save leaflet maps
#----------------------------
# Load custom theme for plots
#library(shiny)
library(mapview)
library(viridis)
library(sf)
library(leaflet)
library(htmlwidgets)

# set map projection
wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84"


# Make simple feature geometry for nest location
sf.prey <- 
  prey %>%
  st_as_sf(coords = c('lon', 'lat'), crs = wgs84)

# Create the map for the specific animal
prey_map <- 
  sf.prey %>%
  mapview(zcol = "site",               
          cex = 5, lwd = 0.5, alpha = 1,
          col.regions = viridis::plasma(n = 20),
          na.color = "transparent",
          map.types = 'Esri.WorldImagery',
          crs = wgs84) 

prey_map

# Convert the mapview object to a leaflet map
prey_map2 = prey_map@map %>%
  addCircles(data = sf.prey,
             lng = ~ (st_coordinates(sf.prey)[,1]),   #nest long
             lat = ~ (st_coordinates(sf.prey)[,2]),   # nest lat
             radius = c(200),   # radius from nest, in meter                                
             color = "steelblue4",     
             fillColor = "steelblue",
             fillOpacity = 0.4,
             stroke = TRUE,
             opacity = 0.5,
             weight = 2) 
# #%>%
#   addCircles(data = sf.prey,
#              lng = ~ (st_coordinates(sf.prey)[,1]),   #nest long
#              lat = ~ (st_coordinates(sf.prey)[,2]),   # nest lat
#              radius = c(250),   # radius from nest, in meter                                
#              color = "steelblue1",     
#              fillColor = "steelblue",
#              fillOpacity = 0.4,
#              stroke = TRUE,
#              opacity = 0.5,
#              weight = 2)
prey_map2

# Make simple feature geometry for nest location
p_locs <- 
  dat %>%
  st_as_sf(coords = c('decimal_longitude', 'decimal_latitude'), crs = wgs84)

# Add the points to the existing map
prey_map3 <- prey_map2 %>%
  addCircleMarkers(data = p_locs, 
                   lng = ~st_coordinates(p_locs)[,1], 
                   lat = ~st_coordinates(p_locs)[,2],
                   radius = 1, color = "red", fill = TRUE, fillColor = "blue", fillOpacity = 0.5)

prey_map3


library(htmlwidgets)
click_script <- "
function(el, x) {
  this.on('click', function(e) {
    var coord = e.latlng;
    var lat = coord.lat;
    var lng = coord.lng;
    console.log('Latitude: ' + lat + ', Longitude: ' + lng);
    if (typeof Shiny !== 'undefined') {
      Shiny.onInputChange(el.id + '_click', [lat, lng]);
    }
  });
}
"

prey_map4 <- prey_map3 %>%
  onRender(click_script)

library(shiny)

# Create a Shiny app to handle the click event
ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    prey_map4
  })
  
  observeEvent(input$map_click, {
    coords <- input$map_click
    cat("Latitude:", coords[1], "Longitude:", coords[2], "\n")
  })
}

shinyApp(ui, server)

