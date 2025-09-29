
# Load libraries
library(sf)
library(dplyr)
library(ggplot2)

# -----------------------------
# Example GPS data
# -----------------------------
dat <- data.frame(
  track_id = as.character(c(1,1,1,1,1,1, 2,2,2)),
  lat = c(-34.49, -34.501,-34.501, -34.499, -34.52, -34.50, -34.49, -34.495, -34.492),
  lon = c(18.5, 18.505, 18.502, 18.507, 18.51, 18.507, 18.52, 18.525, 18.521),
  date.time = as.POSIXct(c("2025-09-22 10:00","2025-09-22 10:05","2025-09-22 10:15" ,"2025-09-22 10:20" ,"2025-09-22 10:25", "2025-09-22 10:30",
                           "2025-09-22 10:00","2025-09-22 10:10","2025-09-22 10:20"))
)

# -----------------------------
# Example GPS data
# -----------------------------
dat <- data.frame(
  track_id = as.character(c(1,1,1,1,1,1,1, 2,2,2)),
  lat = c(-34.49, -34.501,-34.501, -34.499, -34.52, -34.50,-34.49, -34.49, -34.495, -34.492),
  lon = c(18.5, 18.505, 18.502, 18.507, 18.51, 18.507,18.505, 18.52, 18.525, 18.521),
  date.time = as.POSIXct(c("2025-09-22 10:00",
                           "2025-09-22 11:00",
                           "2025-09-22 12:00",
                           "2025-09-22 13:00",
                           "2025-09-22 14:00",
                           "2025-09-22 15:00",
                           "2025-09-22 16:00",
                           "2025-09-22 10:00",
                           "2025-09-22 11:00",
                           "2025-09-22 12:00"))
)

dat

# Convert to sf (WGS84 lon/lat)
gps_sf <- st_as_sf(dat, coords = c("lon","lat"), crs = 4326)

# -----------------------------
# Define circular site in UTM
# -----------------------------
utm.prj <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

site_center <- st_sfc(st_point(c(18.505, -34.5)), crs = 4326)
site_center_utm <- st_transform(site_center, utm.prj)

# buffer 500 m around the center
circle <- st_buffer(site_center_utm, dist = 500)

# -----------------------------
# Reproject GPS points to UTM
# -----------------------------
gps_utm <- st_transform(gps_sf, utm.prj)

# -----------------------------
# Identify points inside circle
# -----------------------------
gps_utm$inside_circle <- st_within(gps_utm, circle, sparse = FALSE)[,1]

# Order by track_id and time
gps_utm <- gps_utm %>%
  arrange(track_id, date.time)

# -----------------------------
# Plot circle, points, and paths
# -----------------------------
ggplot() +
  geom_sf(data=circle, fill=NA, color="red", size=1.2) +
  geom_path(data=gps_utm,
            aes(x=st_coordinates(gps_utm)[,1],
                y=st_coordinates(gps_utm)[,2],
                color=factor(track_id)),
            linewidth=0.8) +
  geom_sf(data=gps_utm, aes(color=factor(track_id), shape=inside_circle), size=3) +
  theme_minimal() +
  labs(color="Track ID", shape="Inside circle") +
  ggtitle("Animal Tracks and Circle Site (UTM projection)")


gps_utm


#-------------------------------------------
# Recursive check
#-------------------------------------------

# # Define the projections 
# # Set new projection to https://epsg.io/32737
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# 
# site_center
# #To assign a known CRS to spatial data:
# wgs.coord = SpatialPoints(cbind(18.505, -34.5), proj4string=CRS(wgs84))
# 
# # To transform from one CRS to another:
# utm.coord <- spTransform(wgs.coord, CRS(utm.prj))
# 
# KD.locations = data.frame(x = utm.coord$coords.x1, y = utm.coord$coords.x2)

site_center_utm

KD.locations = data.frame(x =-1395583 , y = 5985356) 

#To assign a known CRS to spatial data:
dat.wgs.coord = SpatialPoints(cbind(dat$lon, dat$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
dat.utm.coord <- spTransform(dat.wgs.coord, CRS(utm.prj))

dat$lon.x <- dat.utm.coord$coords.x1
dat$lat.y <- dat.utm.coord$coords.x2

head(dat)


# Have to use UTM instead of lat/lon, and only 4 columns:
temp_petrel = dat  %>%
     dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  KD.visits = getRecursionsAtLocations(temp_petrel, locations = KD.locations, radius = 500) 
  
  KD.visits_ = data.frame(KD.visits$revisitStats)
  
  KD.visits_ 
  
  gps_utm 
  
#  KD.visits_$exitTime[1] - KD.visits_$entranceTime[1]
  