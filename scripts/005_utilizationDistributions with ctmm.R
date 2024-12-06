
#-------------------------
# Fit ctmm UDs 
#----------------------
# ctmm model based on code from Chris Fleming: https://groups.google.com/g/ctmm-user/c/JUJLZI-1GL8 
# parallelization based on code from Ingo  https://groups.google.com/g/ctmm-user/c/22EkOdfNLdM/m/JGqds6qxAAAJ 

library(ctmm)
library(doParallel)
library(foreach) 
library(raster)

# giant petrels - Utilization distributions

## Original script by Ryan Reisinger
## Modified by Chris Oosthuizen

library(tidyverse)


# library(viridis)
# library(ggbeeswarm)    
# library(ggnewscale)

## Custom theme for plots
source("./scripts/000_ggtheme.R")

## Get data
dat <- readRDS("./output/maleGP_terrestrial_foraging_trips.rds")
head(dat)

# --------------------------------
# Data summary
# --------------------------------
# Are there missing data in the df?
colnames(dat)[!complete.cases(t(dat))]  # no

dat %>% 
  group_by(scientific_name) %>%
  summarise(unique_indiv = n_distinct(track_id))  # individuals by species

dat %>% 
  summarise(unique_trips = n_distinct(trip_id))  # total trips

dat %>% 
  group_by(scientific_name) %>%
  summarise(unique_trips = n_distinct(trip_id))  # trips by species

nrow(dat) # number of location estimates

dat %>%
  group_by(track_id) %>%
  dplyr::summarise(n.uplinks= n()) %>%
  arrange(n.uplinks)  #  number of location estimates per individual

sampling_rate = dat %>% 
  group_by(track_id) %>%
  mutate(time_diff_min = difftime(date.time, lag(date.time), units = "mins"))

mean(sampling_rate$time_diff_min, na.rm = T)
median(sampling_rate$time_diff_min, na.rm = T)

#---------------------------------
# Calculate trip travel distances 
#---------------------------------
# Calculate geodesic distance on longitude, latitude input vectors. The unit of distance is meters.
# (corresponds to Great Circle distance of trip package)
# Converted to kilometer
dat = dat %>% 
  group_by(trip_id) %>% 
  mutate(trackdistance = traipse::track_distance(x = decimal_longitude, y = decimal_latitude)/1000) %>%
  ungroup()

#-----------------------------------
## Utilization distributions
#-----------------------------------

# KUD of short trips only
ud_dat = dat
names(ud_dat)

# clean up and select a single breeding stage
ctmm_dat = ud_dat %>% 
  dplyr::select(track_id, date.time, decimal_longitude, decimal_latitude, scientific_name) 

ctmm_dat

# Make grid for UD
# lms <- c(min(ctmm_dat$decimal_longitude, na.rm = T),
#          max(ctmm_dat$decimal_longitude, na.rm = T),
#          min(ctmm_dat$decimal_latitude, na.rm = T) ,
#          max(ctmm_dat$decimal_latitude, na.rm = T) )
# 
# rastgrid <- raster(ext = extent(lms), crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
#              res = 0.0025)
# rastgrid

# have to reproject the raster to UTM to use it!

#--------------------------------------------------------
# Format data for ctmm
#--------------------------------------------------------
# Input data should be in the Movebank format. This requires LAT LON (wgs84) coordinates!
# Define the projections 
# Set new projection to https://epsg.io/32721
utm.prj = "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs "   # Chris UTM Marion
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#To assign a known CRS to spatial data:
wgs.coord = sp::SpatialPoints(cbind(ctmm_dat$decimal_longitude, 
                                    ctmm_dat$decimal_latitude), proj4string=CRS(wgs84))

# To transform from one CRS to another:
utm.coord <- spTransform(wgs.coord, CRS(utm.prj))

ctmm_dat$lon.x <- utm.coord$coords.x1
ctmm_dat$lat.y <- utm.coord$coords.x2
head(ctmm_dat)

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#----------
# SGP 
#----------
sgp_ctmm = ctmm_dat %>% 
  dplyr::rename(id = track_id,
                longitude = decimal_longitude,
                latitude = decimal_latitude, 
                x = lon.x, 
                y = lat.y) %>%
  dplyr::group_by(id) %>% 
  dplyr::arrange(date.time) %>%
  dplyr::filter(scientific_name == "Southern giant petrel") %>%
  ungroup()

# define projection
sgp_DATA <- as.telemetry(sgp_ctmm, projection = 'EPSG:32737')
ctmm::projection(sgp_DATA)

#--------------------------
# Set up ctmm model
#--------------------------
## back-end for foreach function (this works for Windows)
cl <- parallel::makeCluster(detectCores(), outfile = "")
doParallel::registerDoParallel(cl)

#----------------
# Model fitting
#----------------
# create a 'fitting' function
fit_ctmm_function <- function(i){
  GUESS <- ctmm.guess(sgp_DATA[[i]], interactive = F)
  ctmm.select(sgp_DATA[[i]], GUESS, verbose = T, trace = 2)
}

# use the fitting function in a foreach loop and parallel backend by using %dopar%
sgp_FITS <- foreach(i=1:length(sgp_DATA), .packages='ctmm') %dopar% { fit_ctmm_function(i) }
parallel::stopCluster(cl)
#----------------
# Run akde 
#----------------
# Use weights = F for evenly spaced data; weights = T for uneven times

# create a 'akde' function
# ud_function <- function(j){
#   akde(sgp_DATA[[j]], sgp_FITS[[j]][[1]], weights = F,
#                      # grid = list(dr = 250, align.to.origin = T))}
#                      grid = list(rastgrid, align.to.origin = T))}

# sgp_UDS <- foreach(j=1:length(sgp_DATA), .packages='ctmm') %dopar% 
#    {ud_function(j)}

sgp_UDS <- lapply(1:length(sgp_DATA), 
                  function(j) akde(sgp_DATA[[j]], sgp_FITS[[j]], weights = F,
                                #  grid = list(rastgrid, align.to.origin = T))) 
                                   grid = list(dr = 100, align.to.origin = T))) 

#saveRDS(sgp_UDS, "./output/ctmm_sgp_UDS.rds")
#sgp_UDS = readRDS("./output/ctmm_sgp_UDS.rds")

# Whether SP.in = T or SP,in = F does not seem to matter
sgp_PKDE <- pkde(sgp_DATA, sgp_UDS, smooth = T) # distribution of the population
#saveRDS(sgp_PKDE, "./output/ctmm_sgp_PKDE.rds")
#sgp_PKDE = readRDS("./output/ctmm_sgp_PKDE.rds")

plot(sgp_PKDE) 
#plot(sgp_UDS) 

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#----------
# NGP 
#----------
ngp_ctmm = ctmm_dat %>% 
  dplyr::rename(id = track_id,
                longitude = decimal_longitude,
                latitude = decimal_latitude, 
                x = lon.x, 
                y = lat.y) %>%
  dplyr::group_by(id) %>% 
  dplyr::arrange(date.time) %>%
  dplyr::filter(scientific_name == "Northern giant petrel") %>%
  ungroup()

# define projection
ngp_DATA <- as.telemetry(ngp_ctmm, projection = 'EPSG:32737')

#----------------
# Model fitting
#----------------
# create a 'fitting' function
fit_ctmm_function <- function(i){
  GUESS <- ctmm.guess(ngp_DATA[[i]], interactive = F)
  ctmm.select(ngp_DATA[[i]], GUESS, verbose = T, trace = 2)
}

# use the fitting function in a foreach loop and parallel backend by using %dopar%
ngp_FITS <- foreach(i=1:length(ngp_DATA), .packages='ctmm') %dopar% { fit_ctmm_function(i) }

#----------------
# Run akde 
#----------------
# Use weights = F for evenly spaced data; weights = T for uneven times

# create a 'akde' function
# ud_function <- function(j){
#   akde(ngp_DATA[[j]], ngp_FITS[[j]][[1]], weights = F,
# 
# ngp_UDS <- foreach(j=1:length(ngp_DATA), .packages='ctmm') %dopar% 
#   {ud_function(j)}

ngp_UDS <- lapply(1:length(ngp_DATA), 
                  function(j) akde(ngp_DATA[[j]], ngp_FITS[[j]], weights = F,
                                   #  grid = list(rastgrid, align.to.origin = T))) 
                                   grid = list(dr = 100, align.to.origin = T))) 

#saveRDS(ngp_UDS, "./output/ctmm_ngp_UDS.rds")
#ngp_UDS = readRDS("./output/ctmm_ngp_UDS.rds")


# Whether SP.in = T or SP,in = F does not seem to matter
ngp_PKDE <- pkde(ngp_DATA, ngp_UDS, smooth = T) # distribution of the population
#saveRDS(ngp_PKDE, "./output/ctmm_ngp_PKDE.rds")
#ngp_PKDE = readRDS("./output/ctmm_ngp_PKDE.rds")

plot(ngp_PKDE) 
#plot(ngp_UDS) 

#--------------------------------------------------------------------
#--------------------------------------------------------------------

sgp_MEAN <- mean(sgp_UDS) # distribution of the sample
sgp_EXT <- extent(sgp_MEAN)

ngp_MEAN <- mean(ngp_UDS) # distribution of the sample
ngp_EXT <- extent(ngp_MEAN)

plot(sgp_DATA, sgp_MEAN, col = "red", ext = sgp_EXT); title('mean()')
plot(ngp_DATA, ngp_MEAN, col = "yellow", ext = ngp_EXT); title('mean()')

summary(sgp_MEAN)
summary(ngp_MEAN)

#--------------------------------------------------------------------
# Exporting the UD boundary countours as a SpatialPolygonsDataFrame
#--------------------------------------------------------------------
sgp_udpoly = SpatialPolygonsDataFrame.UD(sgp_MEAN, level.UD = 0.95, level = 0.95) 
raster::plot(sgp_udpoly) 
# Three UD polygons are given: the mean estimate, and 95 % confidence intervals for the polygon. You can subset this as follows:
# You have to select the name using
sgp_udpoly$name
sgp_udpoly$name[2]   # this should be the est, not the low or high names.
sgp_udpoly_est <- sgp_udpoly[sgp_udpoly$name == sgp_udpoly$name[2] ,]
lines(sgp_udpoly_est, col = "red")
saveRDS(sgp_udpoly, "./output/ctmm_sgp_udpoly.rds")
saveRDS(sgp_udpoly_est, "./output/ctmm_sgp_udpoly_est.rds")

#--------------------------------------------------------------------
# Exporting the UD boundary contours as a SpatialPolygonsDataFrame
#--------------------------------------------------------------------
ngp_udpoly = SpatialPolygonsDataFrame.UD(ngp_MEAN, level.UD = 0.95, level = 0.95) 
raster::plot(ngp_udpoly) 
ngp_udpoly$name
ngp_udpoly$name[2]   # this should be the est, not the low or high names.
ngp_udpoly_est <- ngp_udpoly[ngp_udpoly$name == ngp_udpoly$name[2] ,]
lines(ngp_udpoly_est, col = "red")
saveRDS(ngp_udpoly, "./output/ctmm_ngp_udpoly.rds")
saveRDS(ngp_udpoly_est, "./output/ctmm_ngp_udpoly_est.rds")
#--------------------------------------------------------------------

#-----------------------------------
# Exporting the results as a raster
#-----------------------------------
# raster exports UD object point-estimates distribution functions (DF) to raster objects. 
# DF ="CDF" gives the cumulative probability per cell, which is the one you are after

library(raster)
r <- raster(MEAN, DF="CDF")
raster::plot(r)
r
UDcontour = rasterToContour(r, levels = 0.95)   
raster::plot(UDcontour, add = T, col = "blue")

# Set UD > 95 % to NA so that it does not plot
r[r > 0.95] <- NA
r
raster::plot(r)

# CAREFUL! IN THESE PLOTS, THE CONTOUR MOVES AROUND AS YOU CHANGE THE RASTER IMAGE LENGTH AND HEIGHT!
# But overall it seems to work fine.


#--------------------------------------------------------------------
# library(rgdal)
# # save
# writeOGR(obj=udpoly_est, dsn="./output/ctmm/shapefiles", layer="HPinc95",
#          driver="ESRI Shapefile", overwrite = T)  
#-----------------------------------------
# plot in leaflet
#-----------------------------------------
library(leaflet)
library(sp)

# plot est only
# leaflet plots in lat lon, so have to reproject:
sgp_udpoly_est_latlon = spTransform(sgp_udpoly_est, CRS = 'EPSG:4326')
ngp_udpoly_est_latlon = spTransform(ngp_udpoly_est, CRS = 'EPSG:4326')

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = sgp_udpoly_est_latlon, weight = 2, fillColor = "NA", color = "red") 

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = ngp_udpoly_est_latlon, weight = 2, fillColor = "NA", color = "red") 

parallel::stopCluster(cl)

