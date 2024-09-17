
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
library(argosfilter)
library(ggspatial)
library(geosphere)

# Load custom theme for plots
source("./scripts/000_ggtheme.R")

#--------------------------------
# Get data
#--------------------------------
dat = read.csv('./output/GiantPetrelMales_raw_processed.csv')
met = read.csv('./output/GiantPetrelMales_raw_processed_metadata.csv')

# format date as POSIXct
dat$date.time <- as.POSIXct((dat$datetime), tz = "GMT")

# Check how many individuals (males) there are per species:
dat %>% 
  group_by(sp_code) %>% 
  summarize(samplesize = n_distinct(track_id))  # one more NGP male as in Reisinger paper - the 'brooding?' animal 

dat.raw.locations = dat

# --------------------------------
# Speed filter GPS points:
# --------------------------------

ids <- unique(dat$individual_id)

fn <- dat[0, ]
for (i in 1:length(ids)) {
  print(ids[i])
  f <- dat[dat$individual_id == ids[i], ]
  f$date.time <- paste(f$date, f$time, sep = " ")
  f$date.time <- as.POSIXct(f$date.time, format = "%Y/%m/%d %H:%M:%S", tz = "GMT")
  if (nrow(f) > 5) {
    f$Speed.flag <- vmask(lat = f$decimal_latitude,
                          lon = f$decimal_longitude,
                          dtime = f$date.time,
                          vmax = 30)
    f <- f[f$Speed.flag != "removed", ]
    fn <- rbind.data.frame(fn, f)
  }
}

## Replace dat
dat <- fn
rm(fn)

dim(dat.raw.locations)
dim(dat)  # removed 2 locations

# aside-----------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple exploratory plotting of tracks based on 
# A Guide to Crawl-ing with R - A book based on the 'learning to crawl' workshop(s)
# Josh M. London and Devin S. Johnson
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set a projection 
prj = " +proj=longlat + ellps = WGS84 + datum=WGS84"  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert df object to an sf object
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf_locs.clean <- sf::st_as_sf(dat, coords = c("decimal_longitude","decimal_latitude")) %>% 
  sf::st_set_crs(prj)

# every one of the 8 tracking rounds (stage) is a different colour
# graph not very clear though, as things get plotted over each other.
sf_lines.clean <- sf_locs.clean %>% 
  dplyr::arrange(track_id, sp_code) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs.clean$sp_code))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(ID = as.factor(unique(sf_locs.clean$sp_code)))

ggplot() + 
  layer_spatial(sf_lines.clean, size = 0.75,aes(color = ID)) +
  scale_x_continuous(expansion(mult = c(.6, .6))) +
  scale_color_brewer(palette = "Dark2") +
  gg_theme() +
  theme(legend.position = "bottom") +
  ggtitle("NGP and SGP filtered data")
# aside-----------------------------------------------------------------------------

#------------leaflet maps start------------------------------------------------------

#----------------------------
# Plot and save leaflet maps
#----------------------------

# # Load custom theme for plots
# source("./scripts/000_function to save mapview maps as html_nest radius.R")
# 
# library(shiny)
# library(mapview)
# library(viridis)
# library(sf)
# library(leaflet)
# library(htmlwidgets)
# 
# # set map projection
# wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84"
# 
# # Assuming `dat.all` is your data frame and `ID` is the column with animal IDs
# unique_ids <- unique(dat$individual_id)
# 
# # Directory to save the maps
# output_dir <- "./maps/leaflet_locations_filtered/"  # Change this to your desired directory
# 
# # Ensure the output directory exists
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir)
# }
# 
# # Loop through each unique animal ID and save the map
# for (animal_id in unique_ids) {
#   save_animal_map(animal_id, dat, output_dir)
# }

#------------leaflet maps end------------------------------------------------------

#---------------------------
# Distance from deployment
#---------------------------
# First update ids
ids <- unique(dat$individual_id)

dat$Distance <- distGeo(p1 = cbind(dat$deployment_decimal_longitude, dat$deployment_decimal_latitude),
                        p2 = cbind(dat$decimal_longitude, dat$decimal_latitude))
dat$Distance <- dat$Distance/1000

# for (i in 1:length(ids)) {
# d <- dat[dat$individual_id == ids[i], ]
# p <- ggplot(data = d, aes(x = date.time, y = Distance, colour = sex)) +
#   geom_point() +
#   labs(title = ids[i])
# print(p)
# }

# Maximum distance
l <- dat
l <- l[0, ]
for (i in 1:length(ids)) {
  tm <- dat[dat$individual_id == ids[i], ]
  m <- max(tm$Distance)
  tm$Maxdist <- rep(m, nrow(tm))
  l <- rbind.data.frame(l, tm)
}

## Replace dat
dat <- l
rm(l)

#-----------------------------------
# Update metadata
met <- dat[!duplicated(dat$individual_id), ]


# Automatically assign trips based on runs of successive home locations
# ======================================================
# What is a trip?
# Here: 200 m and at least 2 locations away from nest
# ======================================================
# Some runs code from:
# https://masterr.org/r/how-to-find-consecutive-repeats-in-r/
# By GUANGMING LANG

# First update ids
ids <- unique(dat$individual_id)

tr2 <- dat
tr2$nest <- "nest"
#tr2[tr2$Distance > 0.200, "nest"] <- "trip"   # $nest = nest and trips
tr2[tr2$Distance > 0.150, "nest"] <- "trip"   # $nest = nest and trips   # 150 m is a trip

tr2$trip <- NA
tr2[tr2$nest == "nest", "trip"] <- 0
tr2[tr2$nest == "trip", "trip"] <- 1          # $trip = nest and trips

tr3 <- tr2[0, ]

for (i in 1:length(ids)) {
  print(ids[i])
  
  d <- tr2[tr2$individual_id == ids[i], ]
  
  # Finds runs of locations away from the nest,
  # call these 'realhome'
  d$realhome <- 1 # at nest
  
  runs <- rle(d$trip)
  myruns <- which(runs$values == 1 & runs$lengths >= 3) # Number of locs away
  
  # Find the end position of the runs
  runs.lengths.cumsum <- cumsum(runs$lengths)
  ends <- runs.lengths.cumsum[myruns]
  
  # Find the end position of the runs
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  
  # Store the indices
  dx <- data.frame("starts" = starts, "ends" = ends)
  
  for(j in 1:nrow(dx)) {
    d[dx$starts[j]:dx$ends[j], "realhome"] <- 0 # away from nest
  }
  
  # Plot to check
  p <- ggplot(data = d,
              aes(x = date.time, y = Distance, colour = as.factor(realhome))) +
    geom_point() +
    labs(title = ids[i])
  # print(p)
  
  ## Save Plot 
  png(file = paste0("./maps/tripdistance/", ids[i], "_realhome.png"), bg = "transparent", res = 300, 
      width = 3000, height = 2000)
  print(p)
  dev.off()
  
  ## Drop initial locations away from nest,
  ## GPS setup?
  ## "realhome" == 0
  if (d[1, "realhome"] == 0) {
    dx <- first(which(d$realhome == 1))
    d <- d[dx:nrow(d), ]
  }
  
  ## Then drop trailing locations away from nest,
  ## walking back to base.
  ## Except "SGP11_KD_SEP_2015", which ends at sea
  if (d[nrow(d), "realhome"] == 0 & ids[i] != "SGP11_KD_SEP_2015"
      & ids[i] != "SGP11_KD_SEP_2015x"& ids[i] != "SGP11_KD_SEP_2015y") {
    dx <- last(which(d$realhome == 1))
    d <- d[1:dx, ]
  }
  
  ## Now trim the leading and trailing at nest locations
  dx.start <- first(which(d$realhome == 0))-1
  dx.end <- last(which(d$realhome == 0))+1
  d <- d[dx.start:dx.end, ]
  
  
  # Plot to check
  p <- ggplot(data = d,
              aes(x = date.time, y = Distance, colour = as.factor(realhome))) +
    geom_point() +
    labs(title = paste(ids[i], "(realhome 1 is at the nest)"))
  # print(p)
  
  ## Save Plot 
  png(file = paste0("./maps/tripdistance/", ids[i], "_realhome_drop_start_end.png"), bg = "transparent", res = 300, 
      width = 3000, height = 2000)
  print(p)
  dev.off()
  
  ##################################################################################
  
  # Identify trips
  #-------------------
  d$tripno <- NA
  
  runs <- rle(d$realhome)
  myruns <- which(runs$values == 0) # Number of locs away
  
  # Find the end position of the runs
  runs.lengths.cumsum <- cumsum(runs$lengths)
  ends <- runs.lengths.cumsum[myruns]
  
  # Find the end position of the runs
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  
  # Store the indices
  dx <- data.frame("starts" = starts, "ends" = ends)
  
  for(k in 1:nrow(dx)) {
    d[dx$starts[k]:dx$ends[k], "tripno"] <- k # away from nest
  }
  
  #-------------------
  # Plot to check
  p <- ggplot(data = d,
              aes(x = date.time, y = Distance, colour = as.factor(tripno))) +
    geom_point() +
    #labs(title = ids[i])
    labs(title = paste(ids[i], "(NA is at the nest)"))
  print(p)
  
  ## Save Plot 
  png(file = paste0("./maps/tripdistance/", ids[i], "_trips.png"), bg = "transparent", res = 300, 
      width = 3000, height = 2000)
  print(p)
  dev.off()
  
  ## Add the output
  tr3 <- rbind(tr3, d)
  
}


#---------------------------------------------------------------------------------
# Split ocean-going trips beyond 20 km 
#---------------------------------------------------------------------------------

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP02_092017" & datetime >= as.POSIXct("2017-10-03 02:58:23") & datetime <= as.POSIXct("2017-10-03 09:59:19") ~ 2,
    individual_id == "NGP02_092017" & datetime >= as.POSIXct("2017-10-03 09:59:19") ~ 3,
    TRUE ~ tripno
  ))

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP09_KD_SEP_2015" & datetime >= as.POSIXct("2015-09-24 09:58:57") & datetime <= as.POSIXct("2015-09-24 13:45:25") ~ 3,
    individual_id == "NGP09_KD_SEP_2015" & datetime >= as.POSIXct("2015-09-26 05:30:15") & datetime <= as.POSIXct("2015-09-26 10:46:30") ~ 4,
    individual_id == "NGP09_KD_SEP_2015" & datetime >= as.POSIXct("2015-09-26 10:46:30") & datetime <= as.POSIXct("2015-09-30 02:10:23") ~ 5,
    individual_id == "NGP09_KD_SEP_2015" & datetime >= as.POSIXct("2015-09-30 02:10:23") & datetime <= as.POSIXct("2015-09-30 07:36:29") ~ 6,
    individual_id == "NGP09_KD_SEP_2015" & datetime >= as.POSIXct("2015-09-30 07:36:29") ~ 7, 
    
    TRUE ~ tripno
  ))

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP15_KD_SEP_2015" & datetime >= as.POSIXct("2015-10-09 05:03:03") & datetime <= as.POSIXct("2015-10-09 12:01:43") ~ 2,
    individual_id == "NGP15_KD_SEP_2015" & datetime > as.POSIXct("2015-10-09 12:01:43") ~ 3,
    TRUE ~ tripno
  ))


# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP05_092017" & datetime >= as.POSIXct("2017-10-02 02:49:25") & datetime <= as.POSIXct("2017-10-05 00:14:44") ~ 3,
    individual_id == "NGP05_092017" & datetime >= as.POSIXct("2017-10-05 00:14:44") ~ 4,
    TRUE ~ tripno
  ))

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP20_KD_SEP_2015" & datetime >= as.POSIXct("2015-10-05 04:23:39") & datetime <= as.POSIXct("2015-10-05 14:30:44") ~ 2,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-05 14:30:44") & datetime <= as.POSIXct("2015-10-09 04:02:04") ~ 3,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-09 04:02:04") & datetime <= as.POSIXct("2015-10-09 09:06:01") ~ 4,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-09 09:06:01") & datetime <= as.POSIXct("2015-10-10 05:28:03") ~ 5,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-10 05:28:03") & datetime <= as.POSIXct("2015-10-11 17:23:46") ~ 6,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-11 17:23:46") & datetime <= as.POSIXct("2015-10-13 01:27:39") ~ 7,
    individual_id == "NGP20_KD_SEP_2015" & datetime > as.POSIXct("2015-10-13 01:27:39") ~ 8,
    
    
    TRUE ~ tripno
  ))

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "NGP22_102017" & datetime < as.POSIXct("2017-10-20 17:26:02") ~ 7,
    individual_id == "NGP22_102017" & datetime >= as.POSIXct("2017-10-20 17:26:02") & datetime <= as.POSIXct("2017-10-21 06:59:00") ~ 5,
    individual_id == "NGP22_102017" & datetime >= as.POSIXct("2017-10-23 04:08:33") & datetime <= as.POSIXct("2017-10-23 08:20:32") ~ 6,
    TRUE ~ tripno
  ))


# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "SGP02_KD_SEP_2015" & datetime >= as.POSIXct("2015-10-05 04:43:10") & datetime <= as.POSIXct("2015-10-05 08:49:51") ~ 6,
    individual_id == "SGP02_KD_SEP_2015" & datetime >= as.POSIXct("2015-10-05 08:49:51") & datetime <= as.POSIXct("2015-10-06 11:40:49") ~ 7,
    TRUE ~ tripno
  ))

# Add trip split points between major terrestrial and at-sea trips over 20 km
tr3 <- tr3 %>%
  mutate(tripno = case_when(
    individual_id == "SGP12_102017" & datetime >= as.POSIXct("2017-10-11 04:41:12") & datetime < as.POSIXct("2017-10-12 07:49:31") ~ 2,
    individual_id == "SGP12_102017" & datetime >= as.POSIXct("2017-10-12 07:49:31") ~ 3,
    TRUE ~ tripno
  ))

#-----------------------------------
# Double check 20 km trip splits
#-----------------------------------

for (i in 1:length(ids)) {
  print(ids[i])
  
  d <- tr3[tr3$individual_id == ids[i], ]
  # Plot to check
  p <- ggplot(data = d,
              aes(x = date.time, y = Distance, colour = as.factor(tripno))) +
    geom_point() + theme_bw() + 
    labs(title = paste(ids[i], "(NA is at the nest)"))
  print(p)
  
  ## Save Plot 
  png(file = paste0("./maps/tripdistance/", ids[i], "_trips_land_ocean.png"), bg = "transparent", res = 300, 
      width = 3000, height = 2000)
  print(p)
  dev.off()
}


#-----------------------------------
# Displacement plots
#-----------------------------------

# Keep a copy for later summary
tr_all <- tr3

#-----------------------------------------------------------------------------
# comment this line out / on whether you want to delete points at nest or not
#----------------------------------------------------------------------------
tr3 <- tr3[tr3$realhome == 0, ]     # run this line if you want to delete GPS points at the nest
tr3$trip_id <- paste0(tr3$individual_id, "_", tr3$tripno)

# ## Maximum distance per trip
trips <- unique(tr3$trip_id)
l <- tr3
l <- l[0, ]
for (i in 1:length(trips)) {
  tm <- tr3[tr3$trip_id == trips[i], ]
  m <- max(tm$Distance, na.rm = T)
  tm$trip.Maxdist <- rep(m, nrow(tm))
  l <- rbind.data.frame(l, tm)
}
tr3 <- l
rm(l)

# alternative method for the above
#tr3_c = tr3 %>%
#    group_by(trip_id) %>%
#    dplyr::summarise(trip.Maxdist = max(Distance))

#tr3 = merge(tr3, tr3_c, 
#                 by.x=c("trip_id", "trip_id"))


## Look at trip distance distribution
tripdists <- unique(tr3$trip.Maxdist)
#hist(tripdists[tripdists < 500], breaks = 100)
#abline(v = c(40), col = "red")
plot(sort(tripdists[tripdists < 100]))
#abline(h = c(40), col = "red")
abline(h = c(20), col = "red")


## Plots
tr4 <- tr3
tr4$lag <- NA
tr4 <- tr4[0, ]

trips <- unique(tr3$trip_id)

for (i in 1:length(trips)) {
  trp <- tr3[tr3$trip_id == trips[i], ]
  tms <- diff(trp$date.time)
  units(tms) <- "hours"
  tms <- c(0, tms)
  
  tms <- cumsum(tms)
  trp$lag <- tms
  tr4 <- rbind.data.frame(tr4, trp)
}


#-------------------------------------------------
# Split ocean (long) and terrestial (short) trips
#-------------------------------------------------

## Long trips only
tr4long <- tr4[tr4$trip.Maxdist > 20, ]

# NGP20_KD_SEP_2015
# This animal did 2 trips to Swartkops (terrestial) that were over 20 km. Add these to the 'short'
# data (since they have a terrestrial focus) and remove them from the 'long' data

# extract these trips
swartkop_trips = tr4long %>% 
  dplyr::filter(trip_id == "NGP20_KD_SEP_2015_2" | 
                trip_id == "NGP20_KD_SEP_2015_6")

# remove the trips from the ocean data
tr4long = tr4long %>% 
           dplyr::filter(trip_id != "NGP20_KD_SEP_2015_2" & 
                         trip_id != "NGP20_KD_SEP_2015_6")

## Short trips only
tr4short <- tr4[tr4$trip.Maxdist < 20, ]

# add swartkop trips to this data 
tr4short = rbind(tr4short , swartkop_trips)

## Combined plot
tr4long$which = "Pelagic foraging trips"
tr4short$which = "Terrestrial and nearshore foraging trips"

tr4sub <- rbind(tr4long, tr4short)

dispBoth <- ggplot(tr4sub, aes(x = lag/24, y = Distance, group = trip_id, colour = sex)) +
  geom_line() +
  facet_wrap(~which, nrow = 2, scales = "free") +
  scale_colour_manual(values = c("#984ea3"),
                      name = "Sex") + 
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(),
                     axis.text = element_text(colour = "black")) +
  labs(x = "Days since deployment", y = "Distance from nest (km)") +
  # scale_x_continuous(limits = c(0, 17)) + 
  theme(legend.position = "none") 

dispBoth

# save plot
pdf("./supplement/displacement_distance_from_nest.pdf",
    width = 7, height = 7,
    useDingbats = FALSE)
dispBoth
dev.off()

#-----------------------------------
# Summary table
#-----------------------------------
# number of individuals in each category:
length(unique(tr4sub[tr4sub$sex == "Male" & tr4sub$which == "Terrestrial and nearshore foraging trips", "individual_id"]))
length(unique(tr4sub[tr4sub$sex == "Male" & tr4sub$which == "Pelagic foraging trips", "individual_id"]))

summary <- tr4sub[ , c("scientific_name", "sp_code", "individual_id", "Culmen_length", "Maxdist", "which")]
summary <- summary[!duplicated(summary$individual_id), ]
summary <- summary[!is.na(summary$individual_id), ]

ids <- unique(summary$individual_id)

## Get start and end dates, and calculate mean latititude
## also indicate trip types and proportion of time on land
summary$date.start <- NA
summary$date.end <- NA
summary$duration <- NA
summary$long.trip <- NA
summary$short.trip <- NA

hold <- summary[0, ]

for (i in 1:nrow(summary)) {
  d1 <- summary[summary$individual_id == ids[i], ]
  d2 <- tr4sub[tr4sub$individual_id == ids[i], ]
  # Start and end date
  d1$date.start <- min(d2$date.time, na.rm = T)
  d1$date.end <- max(d2$date.time, na.rm = T)
  # Duration
  d1$duration <- difftime(d1$date.end, d1$date.start, units = "day")
  # Mean latitude
  # d1$mean.lat <- mean(d2$decimal_latitude, na.rm = T)
  
  # Keep only trips, and then calculate
  d2 <- dplyr::filter(d2, !is.na(tripno))
  
  # Calculate Maxdist by trip
  d2 <- d2 %>%
    group_by(which,tripno) %>%
    summarise(tripmax = max(Distance, na.rm = T))
  
  # Long trips?
  if(any(d2$which == 'Pelagic foraging trips'   )) {
    d1$long.trip <- "Y"
  } else {
    d1$long.trip <- "N"
  }
  
  # Short trips?
  if(any(d2$which == 'Terrestrial and nearshore foraging trips')) {
    d1$short.trip <- "Y"
  } else {
    d1$short.trip <- "N"
  }
  
  hold <- rbind(hold, d1)
}

summary <- hold
summary

table(summary$short.trip)
table(summary$long.trip)

## Write to file
write.csv(summary, file = "./supplement/SummaryTable.csv", row.names = F)

#-----------------------------------
## Plot of tracking periods
#-----------------------------------
summary$date.start2 <- format(summary$date.start, format = "%m-%d")
summary$date.end2 <- format(summary$date.end, format = "%m-%d")
summary$year <- format(summary$date.end, format = "%Y")
summary$individual_id <- factor(summary$individual_id, levels = summary$individual_id[order(summary$date.start)])

# make fake dates (set all to same year) for better plotting
# Extract day and month, and set year to 2024
summary$plot_proxy_start <- as.POSIXct(paste("2024", 
                                             month(summary$date.start), day(summary$date.start), sep = "-"), format = "%Y-%m-%d")

summary$plot_proxy_end <- as.POSIXct(paste("2024", 
                                           month(summary$date.end), day(summary$date.end), sep = "-"), format = "%Y-%m-%d")

p1_labels = data.frame(
  date = as.POSIXct(c('2024-09-18','2024-09-25','2024-10-02','2024-10-9','2024-10-16','2024-10-23')),
  label = c("18 Sept", "25 Sept", "2 Oct", "9 Oct", "16 Oct","23 Oct"))

p1 =  ggplot(data = summary) +
  geom_linerange(aes(x = individual_id, ymin = plot_proxy_start, ymax = plot_proxy_end, colour = scientific_name)) +
  scale_colour_manual(values = c("#4daf4a", "#984ea3"), name = "Species") +
  facet_wrap(~year, ncol = 1, scales = "free_y", drop = T) +
  coord_flip() +
  gg_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_y_datetime(
    breaks = p1_labels$date, # Set breaks at date positions
    labels = p1_labels$label)+   # Set custom labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 8)) + 
  ylab("Date") + 
  xlab("Individual")

print(p1)

pdf("./supplement/tracking_periods_36individuals.pdf", width = 6, height = 8, useDingbats = FALSE)
print(p1)
dev.off()

#---------------------------------------------------
## Plot of tracking periods for males in this study
#----------------------------------------------------
summary_short = subset(summary, summary$short.trip == 'Y')

p2 =  ggplot(data = summary_short) +
  geom_linerange(aes(x = individual_id, ymin = plot_proxy_start, ymax = plot_proxy_end, colour = scientific_name)) +
  scale_colour_manual(values = c("#4daf4a", "#984ea3"), name = "Species") +
  facet_wrap(~year, ncol = 1, scales = "free_y", drop = T) +
  coord_flip() +
  gg_theme() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_y_datetime(
    breaks = p1_labels$date, # Set breaks at date positions
    labels = p1_labels$label)+   # Set custom labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 8)) + 
  ylab("Date") + 
  xlab("Individual")

print(p2)

pdf("./supplement/trackingPeriods_28individuals_instudy.pdf", width = 6, height = 8, useDingbats = FALSE)
print(p2)
dev.off()

#-------------------
# Tidy up and save
#-------------------

# select useful columns
tr4short_trim = dplyr::select(tr4short, scientific_name,
                              track_id, sp_code, deployment_site, deployment_decimal_latitude, 
                              deployment_decimal_longitude, 
                              date, time, decimal_latitude, 
                              decimal_longitude, date.time, Distance, Maxdist, 
                              nest, tripno, trip_id, trip.Maxdist)

locs_per_trip = tr4short_trim %>% 
  group_by(track_id, trip_id) %>% 
  tally() %>% 
  arrange(n) %>%
  ungroup()

locs_per_trip
locs_per_trip = locs_per_trip[, 2:3]

tr4short_trim = merge(tr4short_trim, locs_per_trip, by = "trip_id")
head(tr4short_trim)
# tr4short_trim = subset(tr4short_trim, tr4short_trim$n > 2)  # all > 2

saveRDS(tr4short_trim, "./output/maleGP_terrestrial_foraging_trips.rds")

n_distinct(tr4short_trim$track_id)  # individuals
n_distinct(tr4short_trim$trip_id)   # trips
max(tr4short_trim$trip.Maxdist)   # max distance
mean(tr4short_trim$trip.Maxdist)   # mean max distance


#------------------------------
# Plotting
#------------------------------
# Get island shapefile
island = sf::st_read(dsn = "./GIS", layer = "Islands_Polygonizer")
island

#---------------------
# plot nest sites
#---------------------
nest_dat = tr4short_trim %>% 
  dplyr::select(track_id, deployment_decimal_latitude, deployment_decimal_longitude, scientific_name) %>%
  distinct(.keep_all = T)

nest_locs <- nest_dat %>%
  sf::st_as_sf(coords = c("deployment_decimal_longitude", "deployment_decimal_latitude")) %>% 
  sf::st_set_crs(4326) 

saveRDS(nest_locs, './output/nest_locs.rds')

# Define lon-lat for Marion area 
xmin= 37.55 
xmax= 37.95 
ymax= -46.8
ymin= -47

marionmap = ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("#4daf4c", "#984ea6"), name = "Species") +
  annotate(geom = "text",
           x = 37.735,
           y = -46.9,
           label = "Marion\nIsland",
           colour = "black") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9))

marionmap

marionmap = marionmap + 
  layer_spatial(data = nest_locs, aes(color = scientific_name), size = 1.2, inherit.aes = TRUE) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F)

marionmap

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
  theme(legend.position = "none") +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) 

kildalkeymap = kildalkeymap + 
  layer_spatial(data = nest_locs, aes(color = scientific_name),  size = 1.2, inherit.aes = TRUE) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) + 
  annotate(geom = "text",
           x = 37.842,
           y = -46.967,
           label = "Kildalkey\nBay",
           colour = "black",
           size = 3) 

kildalkeymap

library(patchwork)
marionmap + kildalkeymap

# save plot
pdf("./supplement/colony_nest_locations_28individuals.pdf",
    width = 9, height = 9,
    useDingbats = FALSE)
marionmap + kildalkeymap
dev.off()

#-------------------------------
# Plot all terrestrial trips
#------------------------------
library(sf)
# Define the bounding box
bbox <- sf::st_bbox(island)
bbox
minx <- bbox["xmin"] 
maxx <- bbox["xmax"] 
miny <- bbox["ymin"] 
maxy <- bbox["ymax"] 

# Convert df object to an sf object
sf_locs.clean <- sf::st_as_sf(tr4short_trim, coords = c("decimal_longitude","decimal_latitude")) %>% 
  sf::st_set_crs(prj)

sf_lines.clean <- sf_locs.clean %>% 
  dplyr::arrange(track_id, sp_code) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs.clean$sp_code))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(ID = as.factor(unique(sf_locs.clean$sp_code)))

p3 <- ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") 

p3 = p3 + 
  layer_spatial(sf_lines.clean, size = 0.75,aes(color = ID)) +
  scale_x_continuous(expansion(mult = c(.6, .6))) +
  scale_color_brewer(palette = "Dark2") +
  gg_theme() +
  theme(legend.position = "bottom") +
  ggtitle("NGP and SGP terrestrial foraging trips (20 km)")

p3

#-------------------------------------------------
# Save individual maps - terrestrial trips
#-------------------------------------------------
# What files are there in the data directory? 
ids <- unique(tr4short_trim$track_id)
unique(ids)
n_distinct(ids)

for (current_id in ids) {
  
  temp = tr4short_trim %>%
    filter(track_id == current_id) 
  
  ind_nest_locs = nest_locs %>%
    filter(track_id == current_id) 
  
  sf_lines <- temp %>%
    filter(!is.na(tripno)) %>%
    sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
    sf::st_set_crs(4326) %>% # WGS 84 - WGS84 - EPSG:4326
    dplyr::arrange(track_id, date.time) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("MULTILINESTRING")
  
  # Plot using ggplot2
  p3 <- ggplot(data = island) +
    geom_sf(fill = "grey") +
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black")) +
    labs(x = "Longitude", y = "Latitude") 
  
  p3 = p3 + 
    layer_spatial(sf_lines, size = 0.75, aes(color = trip_id)) +
    theme(legend.position = "right") +
    scale_color_viridis_d(name = "Foraging trip") +
    layer_spatial(ind_nest_locs , size = 1.1, color = "red") #+
  
  ggsave(plot = p3 ,
         bg = 'white',
         filename = paste0("./maps/terrestrial_trips/", current_id, "_terrestrial_20km.png"),width=8,height=6)
}


#-------------------------------------------------
# Save individual maps - pelagic trips
#-------------------------------------------------
# What files are there in the data directory? 
ids <- unique(tr4long$track_id)
unique(ids)
n_distinct(ids)

for (current_id in ids) {
  
  temp = tr4long %>%
    filter(track_id == current_id) 
  
  sf_lines <- temp %>%
    filter(!is.na(tripno)) %>%
    sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
    sf::st_set_crs(4326) %>% # WGS 84 - WGS84 - EPSG:4326
    dplyr::arrange(track_id, date.time) %>% 
    dplyr::group_by(trip_id) %>% 
    dplyr::summarise(do_union = FALSE) %>% 
    sf::st_cast("MULTILINESTRING")
  
  # Plot using ggplot2
  p3 <- ggplot(data = island) +
    geom_sf(fill = "grey") +
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black")) +
    labs(x = "Longitude", y = "Latitude") 
  
  p3 = p3 + 
    layer_spatial(sf_lines, size = 0.75, aes(color = trip_id)) +
    theme(legend.position = "right") +
    scale_color_viridis_d(name = "Foraging trip") 
  
  ggsave(plot = p3 ,
         bg = 'white',
         filename = paste0("./maps/pelagic_trips/", current_id, "_pelagic_20km.png"),width=8,height=6)
}


#-------------------------------------------------
# Save all terrestrial trips
#-------------------------------------------------
# What files are there in the data directory? 
ids <- unique(tr4short_trim$track_id)
n_distinct(ids)

hist(tr4short_trim$Distance)

sf_points <- tr4short_trim %>%
  filter(!is.na(tripno)) %>%
  sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
  sf::st_set_crs(4326) 

# Plot using ggplot2
base_map <- ggplot(data = island) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  labs(x = "Longitude", y = "Latitude") 

p11 = base_map + 
  layer_spatial(sf_points, size = 0.75, aes(color = scientific_name)) +
  theme(legend.position = "right") +
  scale_colour_manual(values = c("#4daf4c", "#984ea6"), name = "Species") +
  theme(legend.position = "none") +
  labs(subtitle = "Terrestrial trips < 20 km") + 
  annotate(geom = "text", x = 37.75,  y = -46.68,
           label = "Northern giant petrel",
           colour = "black", size = 3.5) +
  annotate(geom = "text", x = 37.75,  y = -46.74,
           label = "Southern giant petrel",
           colour = "black", size = 3.5) +
  annotate(geom = "point", x = 37.58,  y = -46.68,
           colour = "#4daf4c", size = 3.5, shape = 16) +
  annotate(geom = "point", x = 37.58,  y = -46.74,
           colour = "#984ea6", size = 3.5, shape = 16) 


p11

#-------------------------------------------------
# Save short pelagic trips
#-------------------------------------------------
# What files are there in the data directory? 
ids <- unique(tr4long$track_id)
unique(ids)
n_distinct(ids)

short_pelagic = tr4long %>%
  filter(Maxdist < 200) 

range(short_pelagic$Maxdist)   
hist(short_pelagic$Distance)

sf_short_pelagic <- short_pelagic %>%
  filter(!is.na(tripno)) %>%
  sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
  sf::st_set_crs(4326) %>% # WGS 84 - WGS84 - EPSG:4326
  dplyr::arrange(track_id, date.time) %>% 
  dplyr::group_by(scientific_name, trip_id) %>% 
  dplyr::summarise(do_union = FALSE) %>% 
  sf::st_cast("MULTILINESTRING")

p12 = base_map + 
  layer_spatial(sf_short_pelagic, size = 0.75, aes(color = scientific_name)) +
  theme(legend.position = "right") + 
  scale_colour_manual(values = c("#4daf4c", "#984ea6"), name = "Species") +
  theme(legend.position = "none") +
  labs(subtitle = "Pelagic trips < 100 km")

p12

#-------------------------------------------------
# Save long pelagic trips
#-------------------------------------------------
long_pelagic = tr4long %>%
  filter(Maxdist >= 200) 

min(long_pelagic$Maxdist)   
hist(long_pelagic$Distance)

sf_long_pelagic <- long_pelagic %>%
  filter(!is.na(tripno)) %>%
  sf::st_as_sf(coords = c("decimal_longitude", "decimal_latitude")) %>% 
  sf::st_set_crs(4326) %>% # WGS 84 - WGS84 - EPSG:4326
  dplyr::arrange(track_id, date.time) %>% 
  dplyr::group_by(scientific_name, trip_id) %>% 
  dplyr::summarise(do_union = FALSE) %>% 
  sf::st_cast("MULTILINESTRING")

p13 = base_map + 
  layer_spatial(sf_long_pelagic, size = 0.75, aes(color = scientific_name)) +
  theme(legend.position = "right") + 
  scale_colour_manual(values = c("#4daf4c", "#984ea6"), name = "Species") +
  theme(legend.position = "none") + 
  labs(subtitle = "Pelagic trips > 200 km") + 
  annotate(geom = "point", x = 37.58,  y = -46.74,
           colour = "grey51", size = 3.5, shape = 16) 

p13

p11 + p12 + p13 + plot_annotation(tag_levels = "A") 

ggsave(plot = p11 + p12 + p13 + plot_annotation(tag_levels = "A"),
       bg = 'white',
       filename = paste0("./figures/terrestrial_pelagic_trips.png"),width=10,height=6)




