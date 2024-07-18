
#------------------------------------------------------------------
# This code is based on the paper 
# Signer and Fieberg (2021), PeerJ, DOI 10.7717/peerj.11031
# A fresh look at an old concept: home-range estimation in a tidy world

# https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html
#------------------------------------------------------------------

# load packages
library(amt)
library(tidyverse)
library(raster)

Sys.setenv(TZ = "GMT")

# -------------------------------------------------------------------------
#### read in the GPS data for a group of penguins
# -------------------------------------------------------------------------
# raw GPS data was processed to trips (locations at the nest removed)
gpsdat = dat

#--------------------------------------------------------
# Format data for ctmm
#--------------------------------------------------------
# Input data should be in the Movebank format. This requires LAT LON (wgs84) coordinates!
# Define the projections 
# Set new projection to https://epsg.io/32721
utm.prj = "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs "   # Chris UTM Marion
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#To assign a known CRS to spatial data:
wgs.coord = sp::SpatialPoints(cbind(gpsdat$decimal_longitude, 
                                    gpsdat$decimal_latitude), proj4string=CRS(wgs84))

# To transform from one CRS to another:
utm.coord <- spTransform(wgs.coord, CRS(utm.prj))

gpsdat$lon.x <- utm.coord$coords.x1
gpsdat$lat.y <- utm.coord$coords.x2
head(gpsdat)
is.data.frame(gpsdat)

#------------------------------------
# make track with amt
#------------------------------------
names(gpsdat)

trk <- gpsdat %>%
       dplyr::select(lon.x, lat.y, date.time, track_id) %>%
       make_track(lon.x, lat.y, date.time, id = track_id, crs = 32637)

#Next, we group the track by id and nest the track.

trk1 <- trk %>% nest(data = -"id")
trk1

# get the data for the first animal
x <- trk1$data[[1]]

# apply the data analysis
x %>% track_resample(rate = minutes(60), tolerance = minutes(5)) %>%
  steps_by_burst()

trk2 <- trk1 %>% 
  mutate(steps = map(data, function(x) 
    x %>% track_resample(rate = minutes(60), tolerance = minutes(5)) %>% steps_by_burst()))

trk2

# create a plot of the step-length distributions.
trk2 %>% 
  dplyr::select(id, steps) %>%
  unnest(cols = steps) %>% 
  ggplot(aes(sl_, fill = factor(id))) + 
  geom_density(alpha = 0.4)
