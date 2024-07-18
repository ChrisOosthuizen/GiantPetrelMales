# Giant petrels - Utilization distributions

## Original script by Ryan Reisinger
## Modified by Chris Oosthuizen

library(tidyverse)
library(ggbeeswarm) 
library(brms)
library(patchwork)

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
## Trip statistics
#------------------------------------
# maximum distance per trip
trip_max = dat %>%
           group_by(scientific_name, track_id, trip_id) %>%
           summarize(max_dist = max(Distance)) %>%
           ungroup()

trip_max

# sum of distances between locations per trip
track_distance = dat %>%
  group_by(scientific_name, track_id, trip_id) %>%
  summarize(trackdistance = sum(trackdistance, na.rm = TRUE)) %>%
  ungroup()

track_distance

#------------------------
# beeswarm plots
#------------------------
# https://z3tt.github.io/beyond-bar-and-box-plots/

# Reorder factor levels
trip_max$scientific_name_rev <- fct_rev(trip_max$scientific_name)  # Reverses the order of levels

g_maxdist <- ggplot(trip_max, aes(x = scientific_name_rev, 
                          y = max_dist, 
                          color = scientific_name , fill = scientific_name )) +
  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Maximum trip distance") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_maxdist = g_maxdist + 
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5)  

beeswarm_maxdist

#------------------------
# track_distance
#------------------------
# Reorder factor levels
track_distance$scientific_name_rev <- fct_rev(track_distance$scientific_name)  # Reverses the order of levels

g_trackdist <- ggplot(track_distance, aes(x = scientific_name_rev, 
                          y = trackdistance, 
                          color = scientific_name , fill = scientific_name )) +
#  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Maximum trip distance") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_trackdist = g_trackdist + 
  stat_summary(
    fun = median, geom = "point", 
    shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .33, alpha = .5)  

beeswarm_trackdist

#---------------------------------------
# Fit models for maximum trip distance
#---------------------------------------

hist(trip_max$max_dist, breaks = 20)

# brms models take a while to run!
cores = parallel::detectCores()

# fit a null model
# mod_trip_max_null <- brm(max_dist ~ 1 + (1 | track_id), 
#                      data = trip_max, family = lognormal())

performance::check_predictions(mod_trip_max_null) 

# fit a lognormal model
mod_trip_max_logn <- brm(max_dist ~ scientific_name + (1 | track_id), 
                     data = trip_max, family = lognormal())

performance::check_predictions(mod_trip_max_logn) 

# model comparison / confirmation
loo_null = loo(mod_trip_max_null)
loo_logn = loo(mod_trip_max_logn)

model_comp = loo_compare(loo_null, loo_logn)
print(model_comp, simplify = FALSE, digits = 2)


# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/
library(tidybayes) 

# Posterior predictions across species
grand_mean <- mod_trip_max_logn  %>% 
  epred_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
              re_formula = NA)

plot_grand_mean <- ggplot(data = grand_mean, 
                   aes(x = .epred, y = scientific_name, 
                   fill = scientific_name)) +
  stat_halfeye(slab_alpha = 0.5) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  labs(x = "Predicted maximum trip distance", 
       y = NULL,
       fill = "Species")+ 
  gg_theme() +
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 8.5, y = 1.9, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 12.5, y = 0.9, label = "Northern giant petrel", colour = "black", size = 4) +
  scale_x_continuous(lim= c(2,18)) 

  # theme(
  #   legend.position = "inside", legend.position.inside = c(1, 1),  # Adjust legend position
  #   legend.justification = c(1, 1),  # Justify legend to bottom-right
  #   legend.box.just = "right",
  #   legend.direction="vertical",
  #   legend.background = element_blank(),
  #   legend.box.background = element_blank(),
  #   legend.key.size = unit(1,"line"),  
  #   legend.title = element_text( size=10), 
  #   legend.text=element_text(size=10))
  
plot_grand_mean 

beeswarm_maxdist / plot_grand_mean 

# save plot
pdf("./figures/trip_maxdistance.pdf", width = 5, height = 7, useDingbats = FALSE)
beeswarm_maxdist / plot_grand_mean 
dev.off()

#-------------------------------------------
# Fit models for cumulative track distance
#-------------------------------------------

# Define the model with a Student's t-distribution
mod_track_logn <- brm(trackdistance ~ scientific_name + (1 | track_id), 
                     data = track_distance, family = lognormal())

performance::check_predictions(mod_track_logn)

# fit a null model
# mod_track_null <- brm(trackdistance ~ 1 + (1 | track_id), 
#                       data = track_distance, family = lognormal())

performance::check_predictions(mod_track_null)  

# model comparison / confirmation
loo_track_logn = loo(mod_track_logn)
loo_track_null = loo(mod_track_null)

model_comp_track = loo_compare(loo_track_logn, loo_track_null)
print(model_comp_track, simplify = FALSE, digits = 2)


# Posterior predictions across species
grand_mean_track <- mod_track_logn  %>% 
  epred_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
              re_formula = NA)

plot_grand_mean_track <- ggplot(data = grand_mean_track, 
                          aes(x = .epred, y = scientific_name, 
                              fill = scientific_name)) +
  stat_halfeye(slab_alpha = 0.5) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  labs(x = "Predicted maximum trip distance", 
       y = NULL,
       fill = "Species")+ 
  gg_theme() +
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 21, y = 1.9, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 91, y = 0.9, label = "Northern giant petrel", colour = "black", size = 4) +
  scale_x_continuous(lim= c(0,160)) 

# theme(
#   legend.position = "inside", legend.position.inside = c(1, 1),  # Adjust legend position
#   legend.justification = c(1, 1),  # Justify legend to bottom-right
#   legend.box.just = "right",
#   legend.direction="vertical",
#   legend.background = element_blank(),
#   legend.box.background = element_blank(),
#   legend.key.size = unit(1,"line"),  
#   legend.title = element_text( size=10), 
#   legend.text=element_text(size=10))

beeswarm_trackdist / plot_grand_mean_track 

# save plot
pdf("./figures/track_distances.pdf", width = 5, height = 7, useDingbats = FALSE)
beeswarm_trackdist / plot_grand_mean 
dev.off()
