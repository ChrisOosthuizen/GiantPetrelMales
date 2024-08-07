
# Marion Island Giant Petrels - Data subsetting

# Original code by Ryan Reisinger, from the paper:
# Reisinger RR, Carpenter-Kling T, Connan M, Cherel Y, Pistorius PA. 2020. 
# Foraging behaviour and habitat-use drives niche segregation in sibling seabird species. 
# Royal Society Open Science, 7: 200649.

# Edits: Chris Oosthuizen 

#--------------------
# Load libraries
#--------------------
library(tidyverse)

# Load custom theme for plots
source("./scripts/000_ggtheme.R")

#--------------------------------
# Get data
#--------------------------------
dat <- read.csv("./data/GP_tracks_2019-07-01.csv", stringsAsFactors = F)

head(dat)

n_distinct(dat$individual_id) 

loc_points = dat %>% 
  group_by(individual_id) %>%
  tally()

loc_points %>% arrange(n)

# Remove 2 individuals which goes nowhere
dat <- dat[dat$individual_id != "NGP19_092017", ]
dat <- dat[dat$individual_id != "SGP15_KD_SEP_2015", ]
n_distinct(dat$individual_id) 

# housekeeping: remove useless columns
dat = dat %>% dplyr::select(-trip,
                            -location_quality, -latitude_uncertainty_metres, -longitude_uncertainty_metres)

# Some housekeeping
dat$Culmen_length <- as.numeric(dat$Culmen_length)
dat$Culmen_depth <- as.numeric(dat$Culmen_depth)

# Keep only incubating individuals
#----------------------------------------------------------------------
# paper on male GPs:
# There was 1 individual "brooding" (bill measure says a female), and 1 individual
# "brooding?" (a male according to bill measurements). We include the male here.
#----------------------------------------------------------------------
dat <- dat[dat$breeding_stage == "Incubation" | 
           dat$breeding_stage == "incubating"| 
           dat$breeding_stage == "brooding?", ]
unique(dat$breeding_stage)

n_distinct(dat$individual_id) 

# Updated species codes
dat[dat$sp_code == "NG", "sp_code"] <- "NGP"
dat[dat$sp_code == "SG", "sp_code"] <- "SGP"

dat[dat$scientific_name == "Northern Giant Petrel", "scientific_name"] <- "Northern giant petrel"
dat[dat$scientific_name == "Southern Giant Petrel", "scientific_name"] <- "Southern giant petrel"

# Some locations are missing
dat <- dat[!is.na(dat$decimal_latitude) & !is.na(dat$decimal_longitude), ]

# Some deployment lat and lon are text
dat$deployment_decimal_latitude[grepl("S46 56.339", dat$deployment_decimal_latitude)] <- -46.938983
dat$deployment_decimal_longitude[grepl("E37 51.941", dat$deployment_decimal_longitude)] <- 37.865683

dat$deployment_decimal_latitude <- as.numeric(dat$deployment_decimal_latitude)
dat$deployment_decimal_longitude <- as.numeric(dat$deployment_decimal_longitude)

for (i in 1:nrow(dat)) {
  foo <- dat[i, ]
  lon <- foo$deployment_decimal_longitude
  lat <- foo$deployment_decimal_latitude
  if (lon < 0 & lat > 0) {
    lon.new <- lat
    lat.new <- lon
  } else {
    lon.new <- lon
    lat.new <- lat
  }
  if (lat.new > 0) {
    lat.new <- lat.new * -1
  }
  dat[i, "deployment_decimal_longitude"] <- lon.new
  dat[i, "deployment_decimal_latitude"] <- lat.new
}

# Better deployment locations
dat[dat$individual_id == "SGP10_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.96364  
dat[dat$individual_id == "SGP10_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85145

dat[dat$individual_id == "SGP09_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.96360 
dat[dat$individual_id == "SGP09_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85158

dat[dat$individual_id == "SGP06_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.96381 
dat[dat$individual_id == "SGP06_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85197 

dat[dat$individual_id == "SGP03_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.96048    
dat[dat$individual_id == "SGP03_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85468 

dat[dat$individual_id == "NGP20_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.95499   
dat[dat$individual_id == "NGP20_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.86411

dat[dat$individual_id == "NGP11_26102016", "deployment_decimal_latitude"] <- -46.95397   
dat[dat$individual_id == "NGP11_26102016", "deployment_decimal_longitude"] <- 37.86292

dat[dat$individual_id == "SGP07_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.96361
dat[dat$individual_id == "SGP07_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85207

dat[dat$individual_id == "SGP18_03102016", "deployment_decimal_latitude"] <- -46.96057
dat[dat$individual_id == "SGP18_03102016", "deployment_decimal_longitude"] <- 37.85487

dat[dat$individual_id == "NGP02_092017", "deployment_decimal_latitude"] <- -46.93776
dat[dat$individual_id == "NGP02_092017", "deployment_decimal_longitude"] <- 37.86327

dat[dat$individual_id == "NGP13_26102016", "deployment_decimal_latitude"] <- -46.95383
dat[dat$individual_id == "NGP13_26102016", "deployment_decimal_longitude"] <- 37.86304

dat[dat$individual_id == "NGP05_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.94191   
dat[dat$individual_id == "NGP05_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.87140

dat[dat$individual_id == "NGP17_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.94041
dat[dat$individual_id == "NGP17_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.86775

dat[dat$individual_id == "NGP19_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.95488  
dat[dat$individual_id == "NGP19_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.86420

dat[dat$individual_id == "NGP15_26102016", "deployment_decimal_latitude"] <- -46.96903
dat[dat$individual_id == "NGP15_26102016", "deployment_decimal_longitude"] <- 37.85523

dat[dat$individual_id == "NGP01_KD_SEP_2015", "deployment_decimal_latitude"] <- -46.95285
dat[dat$individual_id == "NGP01_KD_SEP_2015", "deployment_decimal_longitude"] <- 37.85796

dat[dat$individual_id == "SGP06_KD_SEP_2015_21", "deployment_decimal_latitude"] <- -46.96382
dat[dat$individual_id == "SGP06_KD_SEP_2015_21", "deployment_decimal_longitude"] <- 37.85332

dat[dat$individual_id == "SGP12_102017", "deployment_decimal_latitude"] <- -46.96078
dat[dat$individual_id == "SGP12_102017", "deployment_decimal_longitude"] <- 37.85553

dat[dat$individual_id == "SGP02_102017", "deployment_decimal_latitude"] <- -46.96035
dat[dat$individual_id == "SGP02_102017", "deployment_decimal_longitude"] <- 37.85571


# Create metadata
met <- dat[!duplicated(dat$individual_id), ]

# Sex and bill dimensions
met$sex.m <- "Not.sexed"
met[met$track_id == "NGP02_KD_SEP_2015", "sex.m"] <- "Female"
met[met$track_id == "NGP14_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "NGP18_KD_SEP_2015", "sex.m"] <- "Female"
met[met$track_id == "NGP19_KD_SEP_2015", "sex.m"] <- "Male"

met[met$track_id == "SGP01_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "SGP02_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "SGP04_KD_SEP_2015", "sex.m"] <- "Female"
met[met$track_id == "SGP18_KD_SEP_2015", "sex.m"] <- "Female"
met[met$track_id == "SGP20_KD_SEP_2015", "sex.m"] <- "Female"

# Additional sexing
met[met$track_id == "NGP03_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "NGP05_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "NGP011_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "NGP02_26092016", "sex.m"] <- "Female"
met[met$track_id == "NGP05_26102016", "sex.m"] <- "Male"
met[met$track_id == "NGP08_26102016", "sex.m"] <- "Male"
met[met$track_id == "NGP12_26102016", "sex.m"] <- "Female"
met[met$track_id == "NGP13_26102016", "sex.m"] <- "Female"
met[met$track_id == "NGP16_26102016", "sex.m"] <- "Female"
met[met$track_id == "NGP17_26102016", "sex.m"] <- "Female"
met[met$track_id == "NGP21_27102016", "sex.m"] <- "Female"
met[met$track_id == "NGP05_092017", "sex.m"] <- "Male"
met[met$track_id == "NGP19_092017", "sex.m"] <- "Male"

met[met$track_id == "SGP01_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "SGP17_KD_SEP_2015", "sex.m"] <- "Male"
met[met$track_id == "SGP06_03102016", "sex.m"] <- "Female"
met[met$track_id == "SGP07_03102016", "sex.m"] <- "Female"
met[met$track_id == "SGP02_102017", "sex.m"] <- "Male"
met[met$track_id == "SGP09_102017", "sex.m"] <- "Male"
met[met$track_id == "SGP12_102017", "sex.m"] <- "Male"
met[met$track_id == "SGP13_102017", "sex.m"] <- "Male"
met[met$track_id == "SGP23_102017", "sex.m"] <- "Female"
met[met$track_id == "SGP24_102017", "sex.m"] <- "Female"

# Number of individuals sexed molecularly
nrow(met[met$sex.m == "Male" | met$sex.m == "Female", ])

# Plot - Bill dimensions
ggplot(data = met, aes(x = Culmen_length,
                       y = Culmen_depth,
                       shape = sp_code,
                       colour = sex.m)) +
  scale_shape_manual(values = c(16, 17), name = "Species") +
  scale_colour_manual(values = c("#4daf4a", "#984ea3", "#999999"), name = "Molecular sex") +
  labs(x = "Culmen length (mm)", y = "Culmen depth (mm)") +
  geom_point(size = 1.5) +
  gg_theme() +
  geom_vline(xintercept = 97, colour = "grey")

# Sex
dat$sex <- NA
dat[!is.na(dat$Culmen_length) & dat$Culmen_length < 97, "sex"] <- "Female"
dat[!is.na(dat$Culmen_length) & dat$Culmen_length > 97, "sex"] <- "Male"

# Add molecular sex to the data too
dat <- merge(x = dat, y = met[, c("track_id", "sex.m")],
             all.x = T)

# Check for differences
min(met[!is.na(met$Culmen_length) & met$sex == "Male", "Culmen_length"])
max(met[!is.na(met$Culmen_length) & met$sex == "Male", "Culmen_length"])

min(met[!is.na(met$Culmen_length) & met$sex == "Female", "Culmen_length"])
max(met[!is.na(met$Culmen_length) & met$sex == "Female", "Culmen_length"])

min(met$Culmen_depth, na.rm = T)
max(met$Culmen_depth, na.rm = T)

# Assign sex from molecular results
dat[which(is.na(dat$sex)), "sex"] <-
  dat[which(is.na(dat$sex)), "sex.m"]

# Update metadata
met <- dat[!duplicated(dat$individual_id), ]

# And now drop any individuals where sex is still unknown (should be none)
dat <- dat[!is.na(dat$sex), ]
met <- dat[!duplicated(dat$individual_id), ]

#----------------------------------------------------------------------
# paper on male GPs: drop all females
#----------------------------------------------------------------------
unique(dat$sex)

dat <- subset(dat, dat$sex == "Male")
met <- subset(met, met$sex == "Male")

met = met %>% 
       dplyr::select(sp_code, deployment_site, individual_id,
             date,
             deployment_decimal_latitude, deployment_decimal_longitude,
             Culmen_length, Culmen_depth)

head(met)

n_distinct(met$individual_id)

# Check for duplicate rows
duplicate_logical <- duplicated(dat)

# Check if there are any duplicated rows
any(duplicate_logical)

dat = distinct(dat, .keep_all = TRUE)

# Save 
write.csv(dat, './output/GiantPetrelMales_raw_processed.csv')
write.csv(met, './output/GiantPetrelMales_raw_processed_metadata.csv')

#-------------------------------------------------
# plot 
#-------------------------------------------------

# Get island shapefile
library(sf)
library(ggspatial)
island = sf::st_read(dsn = "./GIS", layer = "Islands_Polygonizer")
island

# Define the bounding box (if needed)
bbox <- st_bbox(island)
bbox
xmin <- bbox["xmin"] 
xmax <- bbox["xmax"] 
ymin <- bbox["ymin"] 
ymax <- bbox["ymax"] 

#-----------------------------------------------
# What files are there in the data directory? 
ids <- unique(dat$individual_id)
unique(ids)

# current_id = "NGP02_092017"

for (current_id in ids) {
  
temp = dat %>%
    filter(individual_id == current_id) 
  
sf_locs <- temp %>%
    dplyr::select(deployment_decimal_longitude,deployment_decimal_latitude) %>%
    sf::st_as_sf(coords = c("deployment_decimal_longitude", "deployment_decimal_latitude")) %>% 
    sf::st_set_crs(4326)  %>% # WGS 84 - WGS84 - EPSG:4326
    distinct()

sf_lines <- temp %>%
    sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
    sf::st_set_crs(4326) %>% # WGS 84 - WGS84 - EPSG:4326
    dplyr::arrange(datetime) %>% 
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("MULTILINESTRING")
  
  # Plot using ggplot2
  p3 <- ggplot(data = island) +
    geom_sf(fill = "grey") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black")) +
    labs(x = "Longitude", y = "Latitude") 
  
  
  p3 = p3 + 
    layer_spatial(sf_lines, size = 0.75, color = "red") +
    theme(legend.position = "right") +
    scale_color_viridis_d() +
    layer_spatial(sf_locs , size = 1, color = "red") #+
  
  p3
  
ggsave(plot = p3 ,
         bg = 'white',
         filename = paste0("./maps/all_locations_raw/", current_id, "_raw_locations_.png"),width=8,height=6)
  
}
