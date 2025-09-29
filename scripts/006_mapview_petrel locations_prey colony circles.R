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
gp$Location_type = paste(gp$scientific_name, gp$type)

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
  mapview(zcol = "Location_type",               
          cex = 4, lwd = 0.5, alpha = 1,
          col.regions = c("green", "white", "purple", "red"),
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
             color = "orange",     
             fillColor = "steelblue",
             fillOpacity = 0.4,
             stroke = TRUE,
             opacity = 0.5,
             weight = 2) 

gp_map2

# Save the leaflet map as an HTML file
saveWidget(gp_map2 , "./supplement/leafletmap_gp_locations_preycolony_200mradius.html", selfcontained = TRUE)


#---------------------------------------------------------------------
# plot only prey locations - this converts mapview class to a leaflet map
#---------------------------------------------------------------------
#---------------------------------
# Create map with prey locations
#---------------------------------
gp_map_prey <- gp %>%
  filter(type == "nest") %>%
  mapview(zcol = "Location_type",               
          cex = 4, lwd = 0.5, alpha = 1,
          col.regions = c("green",  "purple"),
          na.color = "transparent",
          map.types = 'Esri.WorldImagery',
          #    map.types = 'Esri.WorldTopoMap',  # Try different map types
          crs = wgs84,
          grid = FALSE,  # Remove grid lines 
          leaflet = TRUE)  # Convert to leaflet map

gp_map_prey

#---------------------------------------------------------------------
# Add prey locations - this converts mapview class to a leaflet map
#---------------------------------------------------------------------
gp_map_prey2 = gp_map_prey@map %>%
  addCircles(data = sf.prey,
             lng = ~ (st_coordinates(sf.prey)[,1]),   #nest long
             lat = ~ (st_coordinates(sf.prey)[,2]),   # nest lat
             radius = c(200),   # radius from nest, in meter                                
             color = "orange",     
             fillColor = "white",
             fillOpacity = 0.4,
             stroke = TRUE,
             opacity = 0.5,
             weight = 2) 

gp_map_prey2

# Save the leaflet map as an HTML file
#saveWidget(gp_map2 , "./supplement/leafletmap_preycolony_200mradius.html", selfcontained = TRUE)


# Add animations of tracks
# Take 1 animal's data only

unique(dat$track_id)

dat1 = dat %>% 
  filter(track_id == "NGP01_KD_SEP_2015") %>%
  arrange(date.time) %>%
  dplyr::select(lon.x, lat.y, date.time, track_id)

head(dat1)

# convert to sf (using your UTM proj string), then transform to WGS84 lon/lat for leaflet:
dat1_sf <- st_as_sf(dat1, coords = c("lon.x", "lat.y"), crs = utm.prj) %>%
  st_transform(crs = 4326)        # EPSG:4326 - lat/lon for leaflet

dat1_sf$label <- as.character(dat1_sf$date.time)
dat1_sf$Name <- as.character(dat1_sf$track_id)
dat1_sf$time <- as.character(dat1_sf$date.time)

# check
head(dat1_sf)

leaflet() %>%
  addTiles() %>%
  addPlayback(
    data = dat1_sf, label = ~label,
    popup = ~ sprintf(
      "I am a popup for <b>%s</b> and <b>%s</b>",
      Name, label
    ),
    popupOptions = popupOptions(offset = c(0, -35)),
    options = playbackOptions(
      radius = 3,
      tickLen = 36000,
      speed = 50,
      maxInterpolationTime = 1000
    ),
    pathOpts = pathOptions(weight = 5)
  )



###################################################



# Add points to your existing map
gp_map_prey2 %>%
  addCircleMarkers(
    data = dat1,
    lng = ~decimal_longitude,
    lat = ~decimal_latitude,
    radius = 4,           # size of the points
    color = "white",        # border color
    fillColor = "white",    # fill color
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    popup = ~as.character(date.time)  # optional: show timestamp on click
  )


library(leaflet.extras2)

# Convert to sf in WGS84
dat1_sf <- dat1 %>%
  arrange(date.time) %>%
  st_as_sf(coords = c("decimal_longitude","decimal_latitude"), crs = 4326) %>%
  mutate(date.time = as.POSIXct(date.time, tz = "UTC"))

# Add playback animation
gp_map_prey2 %>%
  addPlayback(
    data = dat1_sf,
    time = "date.time",
    popup = ~as.character(date.time),
    label = ~as.character(date.time),
    pathOpts = pathOptions(weight = 2, color = "red"),
    options = playbackOptions(
      radius = 5,
      speed = 1000,         # slower for testing
      playControl = TRUE,
      sliderControl = TRUE,
      dateControl = TRUE
    )
  )


