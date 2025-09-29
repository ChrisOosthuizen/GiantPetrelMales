
# Instructions: 
# Run the 004_utilizationDistributions_RSOSrevision script first. 

# This plots the time per visit for various sites, across the two species. 

#-------------------------------------------
# Recursive behaviour at Kildalkey Bay
#-------------------------------------------

# Get prey location data: 
prey = read.csv('./data/lat_lon_prey_colonies_simple.csv')
head(prey)
dim(prey)

unique(prey$site)

KD = prey %>%
     dplyr::filter(site == "Kildalkey Bay")

KD

#To assign a known CRS to spatial data:
KD.wgs.coord = SpatialPoints(cbind(KD$lon, KD$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
KD.utm.coord <- spTransform(KD.wgs.coord, CRS(utm.prj))

KD$lon.x <- KD.utm.coord$coords.x1
KD$lat.y <- KD.utm.coord$coords.x2

head(KD)

KD.locations = data.frame(x = KD$lon.x, y = KD$lat.y)

KD_list = list()
KD.visits_list = list()
KD_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  KD.visits = getRecursionsAtLocations(temp_petrel, locations = KD.locations, radius = 150) 
  
  KD.visits_ = data.frame(KD.visits$revisitStats)
  KD.visits_list[[current_spp]] = KD.visits_
  
  KD.location.visits = KD.visits$revisits
  KD.location.residenceTime = KD.visits$residenceTime
  
  KD_revisits = cbind(KD, KD.location.visits, KD.location.residenceTime)
  KD_revisits$scientific_name =current_spp
  
  KD_list[[current_spp]] = KD_revisits
  
  KD.visits_$species = current_spp
  
  KD_hist_visits_list[[current_spp]] = KD.visits_
  
  
}

KD_dat = bind_rows(KD_list)
head(KD_dat)

KD_visits_dat = bind_rows(KD_hist_visits_list)
head(KD_visits_dat)

KD_resident_time = KD_visits_dat %>%
      group_by(species) %>%
      summarise(KD_resident_time = sum(timeInside),  # sum of all visits
                KD_visits = max(visitIdx))  # number of rows for the species in KD_visits_dat 

KD_resident_time

min(KD_visits_dat$timeInside)
max(KD_visits_dat$timeInside)

# plot
KD_plot = ggplot(KD_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(KD_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,14, by = 0.5)) +
  geom_histogram(data = subset(KD_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,14, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Kildalkey Bay") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(breaks = seq(0, 14, by = 2), labels = seq(0, 14, by = 2)) + 
  scale_y_continuous(limits = c(-22, 22), breaks = seq(-25, 25, by = 5), labels = abs) 

KD_plot 

KD_visits_dat


#-------------------------------------------
# Recursive behaviour at Landfall Beach
#-------------------------------------------
unique(prey$site)

LF = prey %>%
  dplyr::filter(site == "Landfall Beach")

LF

#To assign a known CRS to spatial data:
LF.wgs.coord = SpatialPoints(cbind(LF$lon, LF$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
LF.utm.coord <- spTransform(LF.wgs.coord, CRS(utm.prj))

LF$lon.x <- LF.utm.coord$coords.x1
LF$lat.y <- LF.utm.coord$coords.x2

head(LF)

LF.locations = data.frame(x = LF$lon.x, y = LF$lat.y)

LF_list = list()
LF.visits_list = list()
LF_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  LF.visits = getRecursionsAtLocations(temp_petrel, locations = LF.locations, radius = 150) 
  
  LF.visits_ = data.frame(LF.visits$revisitStats)
  LF.visits_list[[current_spp]] = LF.visits_
  
  LF.location.visits = LF.visits$revisits
  LF.location.residenceTime = LF.visits$residenceTime
  
  LF_revisits = cbind(LF, LF.location.visits, LF.location.residenceTime)
  LF_revisits$scientific_name =current_spp
  
  LF_list[[current_spp]] = LF_revisits
  
  LF.visits_$species = current_spp
  
  LF_hist_visits_list[[current_spp]] = LF.visits_
  
  
}

LF_dat = bind_rows(LF_list)
head(LF_dat)

LF_visits_dat = bind_rows(LF_hist_visits_list)
head(LF_visits_dat)


LF_resident_time = LF_visits_dat %>%
  group_by(species) %>%
  summarise(LF_resident_time = sum(timeInside),  # sum of all visits
            LF_visits = max(visitIdx))  # number of rows for the species in LF_visits_dat 

LF_resident_time

min(LF_visits_dat$timeInside)
max(LF_visits_dat$timeInside)

# plot
LF_plot = ggplot(LF_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(LF_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,15, by = 0.5)) +
  geom_histogram(data = subset(LF_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,15, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Landfall Beach") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 15) ,breaks = seq(0, 14, by = 2), labels = seq(0, 14, by = 2)) +
  scale_y_continuous(limits = c(-7, 17), breaks = seq(-10, 40, by = 5), labels = abs)  

LF_plot

LF_visits_dat


#-------------------------------------------
# Recursive behaviour at Bullard South
#-------------------------------------------
unique(prey$site)

BS = prey %>%
  dplyr::filter(site == "Bullard South")

BS

#To assign a known CRS to spatial data:
BS.wgs.coord = SpatialPoints(cbind(BS$lon, BS$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
BS.utm.coord <- spTransform(BS.wgs.coord, CRS(utm.prj))

BS$lon.x <- BS.utm.coord$coords.x1
BS$lat.y <- BS.utm.coord$coords.x2

head(BS)

BS.locations = data.frame(x = BS$lon.x, y = BS$lat.y)

BS_list = list()
BS.visits_list = list()
BS_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  BS.visits = getRecursionsAtLocations(temp_petrel, locations = BS.locations, radius = 150) 
  
  BS.visits_ = data.frame(BS.visits$revisitStats)
  BS.visits_list[[current_spp]] = BS.visits_
  
  BS.location.visits = BS.visits$revisits
  BS.location.residenceTime = BS.visits$residenceTime
  
  BS_revisits = cbind(BS, BS.location.visits, BS.location.residenceTime)
  BS_revisits$scientific_name =current_spp
  
  BS_list[[current_spp]] = BS_revisits
  
  BS.visits_$species = current_spp
  
  BS_hist_visits_list[[current_spp]] = BS.visits_
  
  
}

BS_dat = bind_rows(BS_list)
head(BS_dat)

BS_visits_dat = bind_rows(BS_hist_visits_list)
head(BS_visits_dat)

BS_resident_time = BS_visits_dat %>%
  group_by(species) %>%
  summarise(BS_resident_time = sum(timeInside),  # sum of all visits
            BS_visits = max(visitIdx))  # number of rows for the species in BS_visits_dat 

BS_resident_time

min(BS_visits_dat$timeInside)
max(BS_visits_dat$timeInside)

# plot
BS_plot = ggplot(BS_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(BS_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,20, by = 0.5)) +
  geom_histogram(data = subset(BS_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,20, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Bullard South") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), labels = seq(0, 20, by = 5)) + 
  scale_y_continuous(limits = c(-7, 10), breaks = seq(-10, 20, by = 5), labels = abs) 

BS_plot

BS_visits_dat


#-------------------------------------------
# Recursive behaviour at Sealers Cave
#-------------------------------------------
unique(prey$site)

SC = prey %>%
  dplyr::filter(site == "Sealers cave")

SC

#To assign a known CRS to spatial data:
SC.wgs.coord = SpatialPoints(cbind(SC$lon, SC$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
SC.utm.coord <- spTransform(SC.wgs.coord, CRS(utm.prj))

SC$lon.x <- SC.utm.coord$coords.x1
SC$lat.y <- SC.utm.coord$coords.x2

head(SC)

SC.locations = data.frame(x = SC$lon.x, y = SC$lat.y)

SC_list = list()
SC.visits_list = list()
SC_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  SC.visits = getRecursionsAtLocations(temp_petrel, locations = SC.locations, radius = 150) 
  
  SC.visits_ = data.frame(SC.visits$revisitStats)
  SC.visits_list[[current_spp]] = SC.visits_
  
  SC.location.visits = SC.visits$revisits
  SC.location.residenceTime = SC.visits$residenceTime
  
  SC_revisits = cbind(SC, SC.location.visits, SC.location.residenceTime)
  SC_revisits$scientific_name =current_spp
  
  SC_list[[current_spp]] = SC_revisits
  
  SC.visits_$species = current_spp
  
  SC_hist_visits_list[[current_spp]] = SC.visits_
  
  
}

SC_dat = bind_rows(SC_list)
head(SC_dat)

SC_visits_dat = bind_rows(SC_hist_visits_list)
head(SC_visits_dat)

SC_resident_time = SC_visits_dat %>%
  group_by(species) %>%
  summarise(SC_resident_time = sum(timeInside),  # sum of all visits
            SC_visits = max(visitIdx))  # number of rows for the species in SC_visits_dat 

SC_resident_time

min(SC_visits_dat$timeInside)
max(SC_visits_dat$timeInside)

# plot
SC_plot = ggplot(SC_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(SC_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,13, by = 0.5)) +
  geom_histogram(data = subset(SC_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,13, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Sealer's Cave") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2), labels = seq(0, 12, by = 2)) + 
  scale_y_continuous(limits = c(-5, 7), breaks = seq(-10, 20, by = 5), labels = abs) 

SC_plot

SC_visits_dat


#-------------------------------------------
# Recursive behaviour at Whale Bird Point
#-------------------------------------------
unique(prey$site)

WB = prey %>%
  dplyr::filter(site == "Whale Bird Point")

WB

#To assign a known CRS to spatial data:
WB.wgs.coord = SpatialPoints(cbind(WB$lon, WB$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
WB.utm.coord <- spTransform(WB.wgs.coord, CRS(utm.prj))

WB$lon.x <- WB.utm.coord$coords.x1
WB$lat.y <- WB.utm.coord$coords.x2

head(WB)

WB.locations = data.frame(x = WB$lon.x, y = WB$lat.y)

WB_list = list()
WB.visits_list = list()

WB_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  WB.visits = getRecursionsAtLocations(temp_petrel, locations = WB.locations, radius = 150) 
  
  WB.visits_ = data.frame(WB.visits$revisitStats)
  WB.visits_list[[current_spp]] = WB.visits_
  
  WB.location.visits = WB.visits$revisits
  WB.location.residenceTime = WB.visits$residenceTime
  
  WB_revisits = cbind(WB, WB.location.visits, WB.location.residenceTime)
  WB_revisits$scientific_name =current_spp
  
  WB_list[[current_spp]] = WB_revisits
  
  WB.visits_$species = current_spp
  
  WB_hist_visits_list[[current_spp]] = WB.visits_
  
  
}

WB_dat = bind_rows(WB_list)
head(WB_dat)

WB_visits_dat = bind_rows(WB_hist_visits_list)
head(WB_visits_dat)

WB_resident_time = WB_visits_dat %>%
  group_by(species) %>%
  summarise(WB_resident_time = sum(timeInside),  # sum of all visits
            WB_visits = max(visitIdx))  # number of rows for the species in WB_visits_dat 

WB_resident_time

min(WB_visits_dat$timeInside)
max(WB_visits_dat$timeInside)

# plot
WB_plot = ggplot(WB_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(WB_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,40, by = 0.5)) +
  geom_histogram(data = subset(WB_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,40, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Whale Bird Point") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5), labels = seq(0, 40, by = 5)) + 
  scale_y_continuous(limits = c(-7, 10), breaks = seq(-10, 20, by = 5), labels = abs) 

WB_plot

WB_visits_dat


#-------------------------------------------
# Recursive behaviour at Kildalkey hut river
#-------------------------------------------
unique(prey$site)

KR = prey %>%
  dplyr::filter(site == "Kildalkey hut river")

KR

#To assign a known CRS to spatial data:
KR.wgs.coord = SpatialPoints(cbind(KR$lon, KR$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
KR.utm.coord <- spTransform(KR.wgs.coord, CRS(utm.prj))

KR$lon.x <- KR.utm.coord$coords.x1
KR$lat.y <- KR.utm.coord$coords.x2

head(KR)

KR.locations = data.frame(x = KR$lon.x, y = KR$lat.y)

KR_list = list()
KR.visits_list = list()
KR_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  KR.visits = getRecursionsAtLocations(temp_petrel, locations = KR.locations, radius = 150) 
  
  KR.visits_ = data.frame(KR.visits$revisitStats)
  KR.visits_list[[current_spp]] = KR.visits_
  
  KR.location.visits = KR.visits$revisits
  KR.location.residenceTime = KR.visits$residenceTime
  
  KR_revisits = cbind(KR, KR.location.visits, KR.location.residenceTime)
  KR_revisits$scientific_name =current_spp
  
  KR_list[[current_spp]] = KR_revisits
  
  KR.visits_$species = current_spp
  
  KR_hist_visits_list[[current_spp]] = KR.visits_
  
  
}

KR_dat = bind_rows(KR_list)
head(KR_dat)

KR_visits_dat = bind_rows(KR_hist_visits_list)
head(KR_visits_dat)

dim(KR_visits_dat)

KR_resident_time = KR_visits_dat %>%
  group_by(species) %>%
  summarise(KR_resident_time = sum(timeInside),  # sum of all visits
            KR_visits = max(visitIdx))  # number of rows for the species in KR_visits_dat 

KR_resident_time

min(KR_visits_dat$timeInside)
max(KR_visits_dat$timeInside)

# plot
KR_plot = ggplot(KR_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(KR_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,16, by = 0.5)) +
  geom_histogram(data = subset(KR_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,16, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Kildalkey hut river") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2), labels = seq(0, 16, by = 2)) + 
  scale_y_continuous(limits = c(-7, 17), breaks = seq(-10, 20, by = 5), labels = abs) 

KR_plot

KR_visits_dat


#-------------------------------------------
# Recursive behaviour at Green Hill
#-------------------------------------------
unique(prey$site)

GH = prey %>%
  dplyr::filter(site == "Green Hill")

GH

#To assign a known CRS to spatial data:
GH.wgs.coord = SpatialPoints(cbind(GH$lon, GH$lat), proj4string=CRS(wgs84))

# To transform from one CRS to another:
GH.utm.coord <- spTransform(GH.wgs.coord, CRS(utm.prj))

GH$lon.x <- GH.utm.coord$coords.x1
GH$lat.y <- GH.utm.coord$coords.x2

head(GH)

GH.locations = data.frame(x = GH$lon.x, y = GH$lat.y)

GH_list = list()
GH.visits_list = list()
GH_hist_visits_list = list()

for (current_spp in species) {
  
  #  current_spp = "Northern giant petrel"
  
  # Have to use UTM instead of lat/lon, and only 4 columns:
  temp_petrel = petrel  %>%
    dplyr::filter(scientific_name == current_spp) %>%
    dplyr::select(lon.x, lat.y, date.time, track_id) %>%
    arrange(track_id, date.time)  # very important to have the data arranged by time - can get negative values other wise
  
  temp_petrel = as.data.frame(temp_petrel)  # tibble gives error
  
  GH.visits = getRecursionsAtLocations(temp_petrel, locations = GH.locations, radius = 150) 
  
  GH.visits_ = data.frame(GH.visits$revisitStats)
  GH.visits_list[[current_spp]] = GH.visits_
  
  GH.location.visits = GH.visits$revisits
  GH.location.residenceTime = GH.visits$residenceTime
  
  GH_revisits = cbind(GH, GH.location.visits, GH.location.residenceTime)
  GH_revisits$scientific_name =current_spp
  
  GH_list[[current_spp]] = GH_revisits
  
  GH.visits_$species = current_spp
  
  GH_hist_visits_list[[current_spp]] = GH.visits_
  
  
}

GH_dat = bind_rows(GH_list)
head(GH_dat)

GH_visits_dat = bind_rows(GH_hist_visits_list)
head(GH_visits_dat)

dim(GH_visits_dat)

GH_resident_time = GH_visits_dat %>%
  group_by(species) %>%
  summarise(GH_resident_time = sum(timeInside),  # sum of all visits
            GH_visits = max(visitIdx))  # number of rows for the species in GH_visits_dat 

GH_resident_time

min(GH_visits_dat$timeInside)
max(GH_visits_dat$timeInside)

# plot
GH_plot = ggplot(GH_visits_dat, aes(x=timeInside) ) +
  geom_histogram(data = subset(GH_visits_dat, species == "Northern giant petrel"),
                 aes(x = timeInside, y = after_stat(count), fill= species),
                 breaks = seq(0,16, by = 0.5)) +
  geom_histogram(data = subset(GH_visits_dat, species == "Southern giant petrel"),
                 aes(x = timeInside, y = -after_stat(count), fill= species),
                 breaks = seq(0,16, by = 0.5)) +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5"),
                    name = "Green Hill") +
  xlab("Residence time per visit (hr)") + 
  ylab("Count") + 
  gg_theme() +
  theme(
    legend.position = "inside", legend.position.inside = c(0.95, 0.95),  # Adjust legend position
    legend.justification = c(1, 1),  # Justify legend to bottom-right
    legend.box.just = "right",
    legend.direction="vertical",
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(1,"line"),  
    legend.title = element_text( size=13, margin = margin(b =12)), # add space below title
    legend.text=element_text(size=11)) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "black") + 
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2), labels = seq(0, 16, by = 2)) + 
 # scale_y_continuous(limits = c(-7, 7), breaks = seq(-10, 20, by = 2), labels = seq(-10, 20, by = 2)) 
   scale_y_continuous(limits = c(-7, 7), breaks = seq(-10, 20, by = 2), labels = abs) 
  
GH_plot

GH_visits_dat

# plot together
library(patchwork)

KD_plot + BS_plot + LF_plot + GH_plot + SC_plot + WB_plot + KR_plot

# Save Kildalkey
ggsave(plot = KD_plot ,
       bg = 'white',
       filename = "./supplement/residence_per_visit_KD.png", width=7,height=5)

# Save Landfall
ggsave(plot = LF_plot ,
       bg = 'white',
       filename = "./supplement/residence_per_visit_LF.png", width=7,height=5)


# What is the proportion of 'long' visits? Do they they longer at non-foraging sites?

GH_plot

GH_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))


LF_plot

LF_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))


KD_plot 

KD_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))

BS_plot 

BS_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))


WB_plot 

WB_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))

KR_plot 

KR_visits_dat %>%
  group_by(species) %>%
  summarise(
    prop_over6 = mean(timeInside > 6, na.rm = TRUE),
    n_over6    = sum(timeInside > 6, na.rm = TRUE),
    n_total    = sum(!is.na(timeInside)),
    ave        = mean(timeInside, na.rm = TRUE),
    sd     = sd(timeInside, na.rm = TRUE))


#-----------------------------------------------
# Individual plots
#-----------------------------------------------
#-----------------------------------------------
# Individuals going to Kildalkey
#-----------------------------------------------


head(KD_visits_dat)

# Count rows per ID per species
KD_visits_counts <- KD_visits_dat %>%
  group_by(species, id) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(species, n) %>%                # sort by count within each species
  group_by(species) %>%
  mutate(ID_new = 1:n()) %>%             # assign new IDs 1–14
  ungroup()

KD_visits_counts

# Plot
ggplot(KD_visits_counts, aes(x = factor(id), 
                      y = n, fill = species)) +
  geom_col() +
  facet_wrap(~species, scales = "free_x") +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5")) +
  labs(x = "Individual", 
       y = "Number of visits") +
  gg_theme() + 
  theme(legend.position = "none")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# Make an annotation dataset
annot <- KD_visits_counts %>%
  group_by(species) %>%
  summarise(
    x = 1,                        # leftmost bar
    y = 15,            # a bit above tallest bar
    label = unique(species),
    .groups = "drop"
  )

# Plot
KD_ind = ggplot(KD_visits_counts, aes(x = factor(ID_new), 
                             y = n, fill = species)) +
  geom_col() +
  facet_wrap(~species, scales = "free_x") +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5")) +
  labs(x = "Individual", 
       y = "Number of visits") +
  gg_theme() + 
  geom_text(
    data = annot,
    aes(x = x, y = y, label = label),   # use the plain text label
    inherit.aes = FALSE,
    hjust = 0, vjust = 0,
    fontface = "plain"                  # now this will render non-bold
  ) +
  theme(strip.text = element_blank(),
        legend.position = "none")


# save plot
# pdf("./figures/KD_ind.pdf", width = 9.4, height = 4, useDingbats = FALSE)
# KD_ind
# dev.off()

ggsave(plot = KD_ind, bg = 'white',
       filename = "./figures/KD_ind.png", width=9,height=5)


#-----------------------------------------------
# Individuals going to Landfall
#-----------------------------------------------

head(LF_visits_dat)

# Count rows per ID per species
LF_visits_counts <- LF_visits_dat %>%
  group_by(species, id) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(species, n) %>%                # sort by count within each species
  group_by(species) %>%
  mutate(ID_new = 1:n()) %>%             # assign new IDs 1–14
  ungroup()

LF_visits_counts

# Plot
#ggplot(LF_visits_counts, aes(x = factor(ID_new), 
  ggplot(LF_visits_counts, aes(x = factor(id), 
                            y = n, fill = species)) +
  geom_col() +
  facet_wrap(~species, scales = "free_x") +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5")) +
  labs(x = "Individual", 
       y = "Number of visits") +
  gg_theme() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# Make an annotation dataset
annot <- LF_visits_counts %>%
  group_by(species) %>%
  summarise(
    x = 1,                        # leftmost bar
    y = 15,            # a bit above tallest bar
    label = unique(species),
    .groups = "drop"
  )

# Plot
ggplot(LF_visits_counts, aes(x = factor(ID_new), 
                             y = n, fill = species)) +
  geom_col() +
  facet_wrap(~species, scales = "free_x") +
  scale_fill_manual(values = c("Northern giant petrel" = "#4daf4c",
                               "Southern giant petrel" = "#984ea5")) +
  labs(x = "Individual", 
       y = "Number of visits") +
  gg_theme() + 
  geom_text(
    data = annot,
    aes(x = x, y = y, label = label),   # use the plain text label
    inherit.aes = FALSE,
    hjust = 0, vjust = 0,
    fontface = "plain"                  # now this will render non-bold
  ) +
  theme(strip.text = element_blank(),
        legend.position = "none")




