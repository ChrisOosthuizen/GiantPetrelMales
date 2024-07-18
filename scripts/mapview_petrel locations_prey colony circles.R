#----------------------------
# Plot and save leaflet maps
#----------------------------

# load packages
library(tidyverse)
library(mapview)
library(sf)
library(leaflet)
library(htmlwidgets)

## Get data
dat <- readRDS("./output/maleGP_terrestrial_foraging_trips.rds")
head(dat)

# set map projection
wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Make simple feature geometry for petrel locations
sf.gp <-  dat %>%
  st_as_sf(coords = c('decimal_longitude', 'decimal_latitude'), crs = wgs84)

sf.gp$type = "trip"

# Make simple feature geometry for nest locations
sf.nest.gp <- dat %>%
  st_as_sf(coords = c('deployment_decimal_longitude', 'deployment_decimal_latitude'), crs = wgs84) %>%
  rename(deployment_decimal_longitude = decimal_longitude, deployment_decimal_latitude = decimal_latitude)

sf.nest.gp$type = "nest"

gp = rbind(sf.gp, sf.nest.gp)
gp$type4 = paste(gp$scientific_name, gp$type)

#-----------------------------
# Specifying prey locations
#-----------------------------
# Get prey location data: 
prey = read.csv('./data/lat_lon_prey_colonies_simple.csv')

# make labels
prey$site_spp = paste0(prey$site,"; ", prey$spp)

# Make simple feature geometry for prey locations
sf.prey <- prey %>%
  st_as_sf(coords = c('lon', 'lat'), crs = wgs84)

#---------------------------------
# Create map with petrel locations
#---------------------------------
gp_map <- gp %>%
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

#---------------------------------------------------------------------
# Add prey locations - this converts mapview class to a leaflet map
#---------------------------------------------------------------------
gp_map2 = gp_map@map %>%
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

gp_map2

# Save the leaflet map as an HTML file
saveWidget(gp_map2 , "./supplement/leafletmap_gp_locations_preycolony_200mradius.html", selfcontained = TRUE)
