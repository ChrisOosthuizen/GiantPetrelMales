
# Marion Island Giant Petrels - Utilization distributions

# Original code by Ryan Reisinger, from the paper:
# Reisinger RR, Carpenter-Kling T, Connan M, Cherel Y, Pistorius PA. 2020. 
# Foraging behaviour and habitat-use drives niche segregation in sibling seabird species. 
# Royal Society Open Science, 7: 200649.

# Edits: Chris Oosthuizen

library(tidyverse)
library(adehabitatHR)
library(raster)
library(viridis)
library(ggbeeswarm)    
library(ggnewscale)
library(ggspatial)
library(sf)
library(patchwork)

## Custom theme for plots
source("./scripts/000_ggtheme.R")

## Get data
dat <- readRDS("./output/maleGP_terrestrial_foraging_trips.rds")
head(dat)

#-----------------------------------
## Utilization distributions
#-----------------------------------

# Make grid for UD
lms <- c(min(dat$decimal_longitude, na.rm = T) - 0.05,
         max(dat$decimal_longitude, na.rm = T) + 0.05,
         min(dat$decimal_latitude, na.rm = T) - 0.05,
         max(dat$decimal_latitude, na.rm = T) + 0.05)

rt <- raster(ext = extent(lms), crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
             res = 0.0025)
rt
rt.sp <- as(rt, "SpatialPixelsDataFrame")

# KUD of short trips only
ud_dat = dat
ud_dat <- ud_dat[ , c("decimal_longitude", "decimal_latitude", "sp_code")]
coordinates(ud_dat) <- c("decimal_longitude", "decimal_latitude")
proj4string(ud_dat) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
kud <- kernelUD(ud_dat, h = 0.001, grid = rt.sp)
image(kud)

# Create rasters of each
vud.males <- getvolumeUD(kud)
image(vud.males)

vud.males.ngp <- raster(as(vud.males$'NGP',"SpatialPixelsDataFrame"))
vud.males.sgp <- raster(as(vud.males$'SGP',"SpatialPixelsDataFrame"))

vud.males.raster <- stack(vud.males.ngp, vud.males.sgp)

names(vud.males.raster) <- c("Northern giant petrel", "Southern giant petrel")
raster::plot(vud.males.raster, col = terrain.colors(100))

# Calculate overlap between UDs method = "UDOI"
overlapUDOI90 <- kerneloverlaphr(kud, method = "UDOI", percent = 90, conditional = T)
overlapUDOI90

overlapUDOI50 <- kerneloverlaphr(kud, method = "UDOI", percent = 50, conditional = T)
overlapUDOI50

kerneloverlaphr(kud, method="HR", percent = 90, conditional=TRUE)
kerneloverlaphr(kud, method="HR", percent = 50, conditional=TRUE)

# Area of UD (NB: have to check whether this is correct based on projection)
kernel.area(kud, percent = c(50, 90),
            unin = c("m"),    
            unout = c("km2"), standardize = FALSE)

## Maps of giant petrel utilization distributions

# NGP Males
ngpm <- rasterToPoints(vud.males.ngp)
ngpm <- data.frame(ngpm)
colnames(ngpm) <- c("lon", "lat", "val")
ngpm$scientific_name <- "Northern giant petrel"
ngpm$sex <- "Male"

# SGP Males
sgpm <- rasterToPoints(vud.males.sgp)
sgpm <- data.frame(sgpm)
colnames(sgpm) <- c("lon", "lat", "val")
sgpm$scientific_name <- "Southern giant petrel"
sgpm$sex <- "Male"

# Bind
kern <- rbind(ngpm, sgpm)
kern <- kern[kern$val <= 90, ]
head(kern)

#------------------------------------------------------
# Polygons for UD (outlines)
#------------------------------------------------------
contour90 <- getverticeshr(kud, percent = 90)
contour50 <- getverticeshr(kud, percent = 50)

# Convert SpatialPolygonsDataFrame to sf object
contour90_sf <- st_as_sf(contour90)
# Filter the sf object for id NGP and SGP
contour90_spp <- contour90_sf %>% 
  filter(id %in% c("NGP", "SGP"))
contour90_spp$scientific_name = c("Northern giant petrel", "Southern giant petrel")

# Convert SpatialPolygonsDataFrame to sf object
contour50_sf <- st_as_sf(contour50)
# Filter the sf object for id NGP and SGP
contour50_spp <- contour50_sf %>% 
  filter(id %in% c("NGP", "SGP"))
contour50_spp$scientific_name = c("Northern giant petrel", "Southern giant petrel")


#---------------------------------------------------------------------
# Make map of Marion 
#---------------------------------------------------------------------
# Define lon-lat for Marion area 
xmin= 37.558 
xmax= 37.945 
ymax= -46.81
ymin= -47

# Get island shapefile
island = sf::st_read(dsn = "./GIS", layer = "Islands_Polygonizer")
# import nest locations
nest_locs = readRDS('./output/nest_locs.rds')

marionmap = ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom") +
  ggplot2::annotate(geom = "text",
           x = 37.735,
           y = -46.9,
           label = "Marion\nIsland",
           colour = "black",
           size = 3) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.98, 1),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(0.7,"line"),  
    legend.title = element_text( size=9), 
    legend.text=element_text(size=9)) + 
    # ggplot2::annotate(geom = "text",
    #                 x = 37.635,
    #                 y = -46.9,
    #                 label = "Swartkop\nPoint",
    #                 colour = "black",
    #                 size = 2) 
  annotate("point", x = 37.575, y = -46.926,
           shape = 10, size = 2, colour = "black")+
  annotate("point", x = 37.77, y = -46.84,
           shape = 2, size = 2, colour = "black")
marionmap

# add UDs
marionmap_UDall = marionmap + 
  geom_tile(data = kern, aes(x = lon, y = lat, fill = val)) +
  facet_wrap(~scientific_name) +
  scale_fill_viridis(direction = 1, option = "plasma",
                     name = "UD (%)", limits = c(0, 100)) +
  labs(x = "Longitude", y = "Latitude") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(size = 9, margin = margin(b = 15))) # Move UD (%) label up
      

marionmap_UDall

# add nests
marionmap_UDall = marionmap_UDall + 
  ggnewscale::new_scale_color() + 
  layer_spatial(data = nest_locs, aes(color = scientific_name), size = 1.2, inherit.aes = TRUE, show.legend = FALSE) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F)+
  #  scale_colour_manual(values = c("#4daf4c", "#984ea6")) + facet_wrap(~scientific_name) + 
  scale_colour_manual(values = c("#4daf4c", "red")) + 
  facet_wrap(~scientific_name)  

marionmap_UDall 
  
# Add 90 or 50 % UD polygon lines
marionmap_UDall = marionmap_UDall + 
  ggnewscale::new_scale_color() + 
   geom_sf(data = contour90_spp, fill = "transparent", color = "brown") + # ,lty = "dotted")+ 
 # geom_sf(data = contour50_spp, fill = "transparent", color = "black")+ 
  facet_wrap(~scientific_name) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) # this keeps the map extent to Marion.

marionmap_UDall

# save plot
# pdf("./figures/utilization_distributions.pdf", width = 9, height = 9, useDingbats = FALSE)
# marionmap_UDall
# dev.off()


#---------------------------------------------------------------------
# Make map of Kildalkey 
#---------------------------------------------------------------------

# Define lon lat for Kildalkey area
xmin= 37.82 
xmax= 37.91 
ymax= -46.89
ymin= -46.985

kildalkeymap = ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  #  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) 

kildalkeymap

# add UDs
kildalkeymapUD = kildalkeymap + 
  geom_tile(data = kern, aes(x = lon, y = lat, fill = val)) +
  facet_wrap(~scientific_name) +
  scale_fill_viridis(direction = 1, option = "plasma",
                     name = "UD (%)", limits = c(0, 100)) +
  labs(x = "Longitude", y = "Latitude") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.title = element_text(size = 9, margin = margin(b = 15)), # Move UD (%) label up
    legend.position = "inside", legend.position.inside = c(0.98, 1),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="horizontal",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(0.7,"line"),  
    legend.text=element_text(size=9))

kildalkeymapUD 

# add nests
kildalkeymapUD = kildalkeymapUD + 
  layer_spatial(data = nest_locs, aes(color = scientific_name),  size = 1.2, inherit.aes = TRUE, show.legend = FALSE) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) + 
  annotate(geom = "text", x = 37.842, y = -46.967,
           label = "Kildalkey\nBay",
           colour = "black", size = 3) + 
  facet_wrap(~scientific_name) +
  scale_colour_manual(values = c("#4daf4c", "red")) 


kildalkeymapUD

# Add 90 or 50 % UD polygon lines
kildalkeymapUD = kildalkeymapUD + 
  ggnewscale::new_scale_color() + 
  geom_sf(data = contour90_spp, fill = "transparent", color = "brown") + # ,lty = "dotted")+ 
  geom_sf(data = contour50_spp, fill = "transparent", color = "black")+ 
  facet_wrap(~scientific_name) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) # this keeps the map extent to Marion.

kildalkeymapUD 

# # save plot
# pdf("./figures/utilization_distributions_kildalkey.pdf", width = 6, height = 6, useDingbats = FALSE)
# kildalkeymapUD
# dev.off()


marionmap_UDall2 = marionmap_UDall + 
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) + 
  theme(plot.margin = margin(1, 1, 1, 1)) + 
  annotation_scale(location = "bl", width_hint = 0.5, text_cex = 0.8,
                   height = unit(0.2, "cm")) 
  
kildalkeymapUD2 = kildalkeymapUD + 
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) +
  theme(strip.text = element_blank()) + 
  annotate(geom = "text", x = 37.866, y = -46.923,
           label = "Bullard North",
           colour = "black", size = 3) + 
  annotate(geom = "text", x = 37.866, y = -46.929,
           label = "Bullard South",
           colour = "black", size = 3) + 
  annotate(geom = "text", x = 37.858, y = -46.944,
           label = "Landfall Beach",
           colour = "black", size = 3) + 
  annotate(geom = "text", x = 37.89, y = -46.901,
           label = "East Cape",
           colour = "black", size = 3) + 
    theme(plot.margin = margin(1, 1, 1, 1)) + 
  annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.8,
                   height = unit(0.2, "cm")) 

combined_plot = marionmap_UDall2 / kildalkeymapUD2 + 
  plot_layout(heights = c(1, 2.15)) & 
  theme(plot.tag.position = c(0.1, 0.98)) & 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12))

combined_plot

# save plot
pdf("./figures/utilization_distributions.pdf", width = 6, height = 6, useDingbats = FALSE)
combined_plot
dev.off()

ggsave(plot = combined_plot, bg = 'white',
       filename = "./figures/utilization_distributions.png", width=6,height=7)


#--------------------------------
# Do this for each individual:
#--------------------------------

# What files are there in the data directory? 
ids <- unique(dat$track_id)
unique(ids)
n_distinct(ids)

for (current_id in ids) {

temp = dat %>%
    filter(track_id == current_id) %>% 
    dplyr::select("track_id", "decimal_longitude", "decimal_latitude")
  
  coordinates(temp) <- c("decimal_longitude", "decimal_latitude")
  proj4string(temp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
  kud <- kernelUD(temp, h = 0.001, grid = rt.sp)

  # Create rasters of each
  vud.males <- getvolumeUD(kud)
  vud.males <- raster(as(vud.males[[1]],"SpatialPixelsDataFrame"))
  
  # SGP Males
  kern <- rasterToPoints(vud.males)
  kern <- data.frame(kern)
  colnames(kern ) <- c("lon", "lat", "val")
  kern$track_id = current_id
  kern <- kern[kern$val <= 90, ]
  head(kern)
  
  #---------------------------------------------------------------------
  # Make map of Marion 
  #---------------------------------------------------------------------
marionmap_ind = marionmap + 
    geom_tile(data = kern, aes(x = lon, y = lat, fill = val)) +
    scale_fill_viridis(direction = 1, option = "plasma",
                       name = "UD (%)",
                       breaks = c(20,40,60,80)) +
    labs(x = "Longitude", y = "Latitude") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    ggplot2::annotate(geom = "text",
             x = 37.65, y = -46.99, label = current_id, colour = "black", size = 3) 
  marionmap_ind
  
  ggsave(plot = marionmap_ind ,
         bg = 'white',
         filename = paste0("./maps/UtilizationDistributions/", current_id, ".png"),width=8,height=6)
}



#----------------------------------------------------------------------
# Recursive analysis with recurse package
#----------------------------------------------------------------------
# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html

require(recurse)
require(sp)
#require(scales)

# don't group; select only
petrel = dat  %>%  
  dplyr::select(scientific_name, decimal_longitude, decimal_latitude,
                date.time, track_id)

#-----------------------------------------------------------
# transform lat / lon to UTM zone 37S  https://epsg.io/32737
#-----------------------------------------------------------

# Define the projections 
#Set new projection to https://epsg.io/32721
utm.prj = "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs "   # Chris UTM Marion
wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

#To assign a known CRS to spatial data:
wgs.coord = sp::SpatialPoints(cbind(petrel$decimal_longitude, 
                                petrel$decimal_latitude), proj4string=CRS(wgs84))
#wgs.coord

# To transform from one CRS to another:
utm.coord <- spTransform(wgs.coord, CRS(utm.prj))
#utm.coord

petrel$lon.x <- utm.coord$coords.x1
petrel$lat.y <- utm.coord$coords.x2
head(petrel)

#--------------------------------------------
# Calculate revisits 
#--------------------------------------------

# Define lon lat for Kildalkey area
xmin= 37.82 
xmax= 37.89 
ymax= -46.92
ymin= -46.985

kildalkeymap = ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9)) 

kildalkeymap

#----------------
# loop
#----------------
species = unique(petrel$scientific_name)

plot_list = list()

for (current_spp in species) {

#current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id)
  
temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
pvisit = getRecursions(temp_petrel, radius = 150)  # radius is in units of the (x,y) location data (meters in the case of a UTM projection)

#plot(pvisit, petrel, legendPos = c(410000, 4740000))
# head(pvisit$revisitStats)

petrel_revisits = as.data.frame(pvisit$revisitStats)
#head(petrel_revisits)

# Add lat lon (WGS84) for plotting
#To assign a known CRS to spatial data:
utm.coord = SpatialPoints(cbind(petrel_revisits$x, petrel_revisits$y), proj4string=CRS(utm.prj))

# To transform from one CRS to another:
wgs.coord <- spTransform(utm.coord, CRS(wgs84))

petrel_revisits$lon <- wgs.coord$coords.x1
petrel_revisits$lat <- wgs.coord$coords.x2
#head(petrel_revisits)

# make sf
petrel_locs <- petrel_revisits %>%
  dplyr::select(lon, lat, visitIdx) %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326) 

temp_nest_locs = nest_locs %>%
                 filter(scientific_name == current_spp)

gpvisits = kildalkeymap +
  ggplot2::annotate(geom = "text",
           x = 37.85,
           y = -46.923, label = current_spp, colour = "black", size = 4) +
  gg_theme()

gpvisits = gpvisits + 
  #layer_spatial(data = nest_locs, aes(color = scientific_name)) +
  #coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) + 
  #scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ggplot2::annotate(geom = "text",
           x = 37.842,
           y = -46.967,
           label = "Kildalkey\nBay",
           colour = "black",
           size = 3) 

gpvisits

gpvisits = gpvisits +
 # ggnewscale::new_scale_color() + 
  layer_spatial(data = petrel_locs, 
                aes(color = visitIdx), size = 1, inherit.aes = TRUE) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) + 
  scale_color_viridis(name = "Revisit index", option = "plasma", limits = c(0, 120)) 

# add nests
gpvisits = gpvisits + 
  ggnewscale::new_scale_color() + 
  layer_spatial(data = temp_nest_locs, aes(color = scientific_name), size = 1.2, inherit.aes = TRUE, show.legend = F) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F)+
  #  scale_colour_manual(values = c("#4daf4c", "#984ea6")) + facet_wrap(~scientific_name) + 
  scale_colour_manual(values = c("red"))  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_list[[current_spp]] = gpvisits

}

#plot_list
ngp_visits = plot_list[[1]] + 
 # theme(axis.text.y = element_blank()) + 
  theme(
    legend.position = "inside", legend.position.inside = c(0.1, 0.9),  # Adjust legend position
    legend.justification = c(0, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=10), 
    legend.text=element_text(size=10)) 

sgp_visits = plot_list[[2]] + 
  ylab("") + 
  theme(axis.text.y = element_blank()) + 
   theme(
     legend.position = "inside", legend.position.inside = c(0.1, 0.9),  # Adjust legend position
      legend.justification = c(0, 1),  # Justify legend to bottom-right
      legend.box.just = "right",
      legend.direction="vertical",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key.size = unit(1,"line"),  
     legend.title = element_text( size=10), 
     legend.text=element_text(size=10))
#sgp_visits

#TIFF
tiff(file = 'figures/revisits.tif', compression = "lzw", antialias = "cleartype",
     width = 10, height = 6, units = "in", res = 800)
ngp_visits + sgp_visits
dev.off()


#-------------------------------------------
# Recursive behaviour at specific locations
#-------------------------------------------
# Get prey location data: 
prey = read.csv('./data/lat_lon_prey_colonies_simple.csv')
head(prey)
dim(prey)

#To prey#To assign a known CRS to spatial data:
prey.wgs.coord = SpatialPoints(cbind(prey$lon, prey$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
prey.utm.coord <- spTransform(prey.wgs.coord, CRS(utm.prj))

prey$lon.x <- prey.utm.coord$coords.x1
prey$lat.y <- prey.utm.coord$coords.x2
head(prey)

prey.locations = data.frame(x = prey$lon.x, y = prey$lat.y)

prey_list = list()
prey.visits_list = list()

for (current_spp in species) {
  
#  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error

prey.visits = getRecursionsAtLocations(temp_petrel, locations = prey.locations, radius = 150) 

prey.visits_ = data.frame(prey.visits$revisitStats)
prey.visits_list[[current_spp]] = prey.visits_

prey.location.visits = prey.visits$revisits
prey.location.residenceTime = prey.visits$residenceTime

prey_revisits = cbind(prey, prey.location.visits, prey.location.residenceTime)
prey_revisits$scientific_name =current_spp

prey_list[[current_spp]] = prey_revisits

}

prey_dat = bind_rows(prey_list)
head(prey_dat)

# indiv_v = bind_rows(prey.visits_list)
# head(indiv_v)
# n_distinct(indiv_v$id)

# --------------------------------
# Plot residence time with labels
# --------------------------------
# labelling is complex because of ggbeeswarm::geom_quasirandom. Simple geom_ doesnt work nicely.

head(prey_dat)

# Reorder factor levels
#trip_max$scientific_name_rev <- fct_rev(trip_max$scientific_name)  # Reverses the order of levels

# make labels
prey_dat$site_spp = paste0(prey_dat$site,"(", prey_dat$spp,")")

# Define what constitutes an outlier, and label only these
threshold <- 18

# Create a new column to identify outliers, for labels
prey_dat <- prey_dat %>%
  mutate(is_outlier = ifelse(prey.location.residenceTime > threshold, TRUE, FALSE))

# plot without labels
g = ggplot(prey_dat, aes(x = scientific_name, 
                         y = prey.location.residenceTime, 
                         color = scientific_name , fill = scientific_name)) +
  #scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Residence time (hour)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

g = g + stat_summary(fun = median, geom = "point", shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5) 

g

# Extract the data with computed positions
g_built <- ggplot_build(g)
quasirandom_data <- g_built$data[[2]]

# Add the computed positions to the original data
prey_dat <- prey_dat %>%
  mutate(x_position = quasirandom_data$x,
         y_position = quasirandom_data$y)

# Create the final plot with labels
g_final <- ggplot(prey_dat, aes(x = x_position, y = y_position, color = scientific_name, fill = scientific_name)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Residence time (hour)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

g_final <- g_final + 
  stat_summary(aes(x = scientific_name, y = prey.location.residenceTime), 
               fun = median, geom = "point", 
               shape = 95, size = 20) +
  geom_point(size = 3, alpha = 0.5) +
  geom_text(aes(label = ifelse(is_outlier, site, NA)), 
            hjust = 0, vjust = 0, size = 3, nudge_x = 0.04)

print(g_final)


# ----------------------------
# Plot revisits with labels
# ----------------------------

# Define what constitutes an outlier
threshold2 <- 50

# Create a new column to identify outliers
prey_dat <- prey_dat %>%
  mutate(is_outlier2 = ifelse(prey.location.visits > threshold2, TRUE, FALSE))


g2 = ggplot(prey_dat, aes(x = scientific_name, 
                          y = prey.location.visits, 
                          color = scientific_name , fill = scientific_name)) +
  #scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Number of repeat visits") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

g2 = g2 + 
  stat_summary(fun = median, geom = "point", shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5) 

# Extract the data with computed positions
g2_built <- ggplot_build(g2)
quasirandom_data <- g2_built$data[[2]]

# Add the computed positions to the original data
prey_dat <- prey_dat %>%
  mutate(x_position = quasirandom_data$x,
         y_position = quasirandom_data$y)

# Create the final plot with labels
g2_final <- ggplot(prey_dat, aes(x = x_position, y = y_position, color = scientific_name, fill = scientific_name)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Number of repeat visits") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

g2_final <- g2_final + 
  stat_summary(aes(x = scientific_name, y = prey.location.visits), 
               fun = median, geom = "point", 
               shape = 95, size = 20) +
  geom_point(size = 3, alpha = 0.5) +
  geom_text(aes(label = ifelse(is_outlier2, site, NA)), 
            hjust = 0, vjust = 0, size = 3, nudge_x = 0.04)

print(g2_final)


# Load the image as a raster object
img <- grid::rasterGrob(png::readPNG("./images/penguin.png"), interpolate = TRUE)

recurse_fig1 = g_final + 
   annotation_custom(img, xmin = 0.41, xmax = 2.55, ymin = 65, ymax = 75) + 
   annotation_custom(img, xmin = 2.40, xmax = 2.65, ymin = 40, ymax = 50) 

recurse_fig2 = g2_final + 
  annotation_custom(img, xmin = 0.57, xmax = 2.55, ymin = 176, ymax = 196) + 
  annotation_custom(img, xmin = 2.40, xmax = 2.65, ymin = 88, ymax = 110) 
  
Rfig = recurse_fig1 + recurse_fig2 + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12))

Rfig

# # save plot
# pdf("./figures/ResidenceTime_RepeatVisits.pdf", width = 9, height = 5, useDingbats = FALSE)
# Rfig
# dev.off()

ggsave(plot = Rfig, bg = 'white',
       filename = "./figures/ResidenceTime_RepeatVisits.png", width=9,height=5)


#-----------------------------------------------------------------
# calculate closest distance between nests and prey colonies:
#-----------------------------------------------------------------
# https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates

library(geosphere)

# Extract nest coordinates
nestcoordinates <- as.data.frame(st_coordinates(nest_locs))

nest_dat = as.data.frame(nest_locs)
nest_dat$lon = nestcoordinates$X
nest_dat$lat = nestcoordinates$Y
head(nest_dat)

# prey data
#prey = read.csv('./data/lat_lon_prey_colonies.csv')
head(prey)
prey$site_spp = paste0(prey$site,"(", prey$spp,")")
head(prey)

# create distance matrix
mat <- distm(prey_dat[,c('lon','lat')], nest_dat[,c('lon','lat')], fun=distVincentyEllipsoid)
mat = mat/1000
mat = as.data.frame(mat)     
ids <- unique(dat$track_id)
unique(ids)
# Replace column names
colnames(mat) <- ids
mat$site_spp = prey$site_spp
print(mat, digits = 1)
str(mat)

nest_prey_dist = mat %>% pivot_longer(names_to = "track_id", values_to = "Distance", 
                                      cols = starts_with("NGP") | starts_with("SGP")) %>%
                          distinct(.keep_all = T)

nest_prey_dist 

nest_prey_dist$spp_code = substr(nest_prey_dist$track_id, 1, 3)
nest_prey_dist

gnest <- ggplot(nest_prey_dist, aes(x = spp_code, 
                                  y = Distance, 
                                  color = spp_code , fill = spp_code )) +
#  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Distance to prey sites") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_gnest = gnest  + 
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5)  

beeswarm_gnest 


#---------------------------------------------
# Distance from nest to Bullard only (example) 
#---------------------------------------------
# NGPs use Bullard more - but they also have nests closer to it.

unique(nest_prey_dist$site_spp)

nest_bullard = nest_prey_dist %>%
             dplyr::filter(site_spp == "Bullard North(macaroni p., ses)")

nest_bull <- ggplot(nest_bullard, aes(x = spp_code, 
                                    y = Distance, 
                                    color = spp_code , fill = spp_code )) +
  #  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Distance to Bullard North (km)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_nest_bull = nest_bull + 
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5)#  +
 # labs(subtitle = "Distance: petrel nests to Bullard North")

beeswarm_nest_bull

#---------------------------------------------
# Distance from nest to Kildalkey only (example) 
#---------------------------------------------

nest_kd = nest_prey_dist %>%
  dplyr::filter(site_spp == "Kildalkey Bay(king p, macaroni p., ses)")

nest_kd <- ggplot(nest_kd, aes(x = spp_code, 
                                      y = Distance, 
                                      color = spp_code , fill = spp_code )) +
  #  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Distance to Kildalkey Bay (km)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_nest_kd = nest_kd + 
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5)  #+
 # labs(subtitle = "Distance: petrel nests to Kildalkey Bay")

beeswarm_nest_kd

ggsave(plot = beeswarm_nest_kd, bg = 'white',
       filename = "./supplement/distance_nest_to_kildalkey.png", width=4,height=4)

ggsave(plot = beeswarm_nest_bull, bg = 'white',
       filename = "./supplement/distance_nest_to_bullard.png", width=4,height=4)



#-----------------------------------------------
# INDIVIDUAL level re-visits
#-----------------------------------------------
indiv_prey_list = list()
indiv_prey.visits_list = list()

# What files are there in the data directory? 
ids <- unique(dat$track_id)
unique(ids)
n_distinct(ids)

for (current_id in ids) {
  
  #current_id = "NGP03_26092016"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(track_id == current_id) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  prey.visits = getRecursionsAtLocations(temp_petrel, locations = prey.locations, radius = 150) 
  
  prey.visits_ = data.frame(prey.visits$revisitStats)
  indiv_prey.visits_list[[current_id]] = prey.visits_
  
  prey.location.visits = prey.visits$revisits
  prey.location.residenceTime = prey.visits$residenceTime
  
  prey_revisits = cbind(prey, prey.location.visits, prey.location.residenceTime)
  prey_revisits$track_id =current_id
  
  indiv_prey_list[[current_id]] = prey_revisits
  
}

indiv_prey_dat = bind_rows(indiv_prey_list)

n_distinct(indiv_prey_dat$track_id)  # 28
n_distinct(nest_prey_dist$track_id) # 28

#-----------------------------------------------
# INDIVIDUAL re-visits and distance to nest
#-----------------------------------------------
head(nest_prey_dist)
head(indiv_prey_dat)

indiv_prey_dat$site_spp = paste0(prey_dat$site,"(", prey_dat$spp,")")

nestdistance_visits = merge(nest_prey_dist, indiv_prey_dat, by = c("track_id", "site_spp"))

head(nestdistance_visits)

unique(nestdistance_visits$site_spp)

kd_visits = nestdistance_visits %>% 
  filter(site_spp == "Kildalkey bay(king p, macaroni p., ses)") %>%
  ggplot(aes(x = Distance,
             y = prey.location.visits,
             color = spp_code)) + 
  geom_point(size = 2) + 
  gg_theme() +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  labs(subtitle = "Distance (petrel nests to Kildalkey bay) vs. location visits")

kd_visits

ggsave(plot = kd_visits, bg = 'white',
       filename = "./supplement/distance_nest_to_Kildalkey_againt_visits_to_Kildalkey.png", width=5,height=5)


kd_visit = nestdistance_visits %>% 
  filter(site_spp == "Kildalkey bay(king p, macaroni p., ses)")

kd_visit %>%
        arrange(prey.location.residenceTime)

kd_visit %>%
  arrange(prey.location.visits)

table(kd_visit$spp_code, kd_visit$prey.location.residenceTime)
table(kd_visit$spp_code, kd_visit$prey.location.visits)


nestdistance_visits %>% 
  ggplot(aes(x = Distance,
             y = prey.location.visits,
             color = spp_code)) + 
  geom_point() + 
  gg_theme() +
  labs(subtitle = "Distance (petrel nests to all sites) vs. location visits")


nestdistance_visits %>% 
  ggplot(aes(x = Distance,
             y = prey.location.residenceTime,
             color = spp_code)) + 
  geom_point() + 
  gg_theme()+
  labs(subtitle = "Distance (petrel nests to all sites) vs. Residence time (hour)")











  