
library(tidyverse)

## Custom theme for plots
source("./scripts/000_ggtheme.R")

## Get data
dat <- readRDS("./output/maleGP_terrestrial_foraging_trips.rds")
head(dat)

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
sf.gp <- 
  dat %>%
  st_as_sf(coords = c('decimal_longitude', 'decimal_latitude'), crs = wgs84)

sf.gp$type = "trip"
  
# Make simple feature geometry for nest location
sf.nest.gp <- 
  dat %>%
  st_as_sf(coords = c('deployment_decimal_longitude', 'deployment_decimal_latitude'), crs = wgs84) %>%
  rename(deployment_decimal_longitude = decimal_longitude, deployment_decimal_latitude = decimal_latitude)

sf.nest.gp$type = "nest"

gp = rbind(sf.gp, sf.nest.gp)
gp$type4 = paste(gp$scientific_name, gp$type)


# Create the map
gp_map <- 
  gp %>%
  mapview(zcol = "type4",               
          cex = 4, lwd = 0.5, alpha = 1,
          col.regions = c("blue", "white", "yellow", "red"),
          na.color = "transparent",
          map.types = 'Esri.WorldImagery',
      #    map.types = 'Esri.WorldTopoMap',  # Try different map types
          crs = wgs84,
          grid = FALSE,  # Remove grid lines 
          leaflet = TRUE)  # Convert to leaflet map

gp_map

# Convert the mapview object to a leaflet map
leaflet_map <- gp_map@map %>%
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
saveWidget(leaflet_map , "./supplement/leafletmap_locations.html", selfcontained = TRUE)
