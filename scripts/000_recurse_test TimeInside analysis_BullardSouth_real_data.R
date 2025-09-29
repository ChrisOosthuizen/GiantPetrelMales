
# Load libraries
library(sf)
library(dplyr)
library(ggplot2)

# -----------------------------
# GP GPS data
# -----------------------------

## Get data
dat <- readRDS("./output/maleGP_terrestrial_foraging_trips.rds")
head(dat)

unique(dat$trip_id)

# Selection 1 trip
# dat = dat %>%
#     dplyr::filter(trip_id == 'NGP01_KD_SEP_2015_1')
# dat = dat %>%
#   dplyr::filter(trip_id ==  "NGP19_KD_SEP_2015_5")

dat = dat %>%
  dplyr::filter(trip_id ==  "SGP12_102017_1")


head(dat)

# Convert to sf (WGS84 lon/lat)
gps_sf <- st_as_sf(dat, coords = c("decimal_longitude","decimal_latitude"), crs = 4326)

# -----------------------------
# Define circular site in UTM
# -----------------------------
utm.prj <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#-------------------------------------------
# Recursive behaviour at Kildalkey Bay
#-------------------------------------------
# Get prey location data: 
prey = read.csv('./data/lat_lon_prey_colonies_simple.csv')
head(prey)
dim(prey)

unique(prey$site)


# KD = prey %>%
#   dplyr::filter(site == "Kildalkey Bay")

KD = prey %>%
  dplyr::filter(site == "Bullard South")

#KD = prey
KD

#site_center <- st_sfc(st_point(c(37.85235, -46.96725 )), crs = 4326)
site_center <- st_sfc(st_point(c(37.88163, -46.927 )), crs = 4326)


site_center_utm <- st_transform(site_center, utm.prj)

# buffer 500 m around the center
circle <- st_buffer(site_center_utm, dist = 150)

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
  geom_sf(data=circle, fill=NA, color="blue", size=1.2) +
  geom_path(data=gps_utm,
            aes(x=st_coordinates(gps_utm)[,1],
                y=st_coordinates(gps_utm)[,2],
                color=factor(track_id)),
            linewidth=0.8,
            alpha = 0.3) +
  geom_sf(data=gps_utm, aes(color=factor(track_id), shape=inside_circle), size=2, alpha = 0.5, color="purple") +
  theme_minimal() +
  labs(color="Track ID", shape="Inside circle") +
  ggtitle("Animal Tracks and Circle Site (UTM projection)") + 
  # crop only to kildalkey circle area
  coord_sf(
    xlim = c(st_bbox(circle)$xmin - 200, st_bbox(circle)$xmax + 200),
    ylim = c(st_bbox(circle)$ymin - 200, st_bbox(circle)$ymax + 200),
    expand = FALSE
  )


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

# KD.locations = data.frame(x = (412696.3 ) , y = 4797836) 

KD.locations = data.frame(x = (414859.9) , y = 4802341) 


# Have to use UTM instead of lat/lon, and only 4 columns:
temp_petrel = dat  %>%
  dplyr::select(lon.x, lat.y, date.time, track_id) %>%
  arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise

temp_petrel = as.data.frame(temp_petrel)  # tibble gives error

KD.visits = getRecursionsAtLocations(temp_petrel, locations = KD.locations, radius = 150) 

KD.visits_ = data.frame(KD.visits$revisitStats)

KD.visits_ 

#write.csv(KD.visits_ , 'KD.visits.csv')
#gps_utm 

#  KD.visits_$exitTime[1] - KD.visits_$entranceTime[1]
