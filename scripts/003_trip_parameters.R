# Marion Island Giant Petrels - modelling trip parameters

# Original code by Chris Oosthuizen 

library(tidyverse)
library(ggbeeswarm) 
library(brms)
library(patchwork)
library(tidybayes) 

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

dat %>% 
  group_by(scientific_name, track_id) %>%
  summarise(unique_trips = n_distinct(trip_id)) %>%
  ungroup() %>%
  group_by(scientific_name) %>%
  summarise(min_unique_trips = min(unique_trips),
            max_unique_trips = max(unique_trips),
            ave_unique_trips = mean(unique_trips),
            sd_unique_trips =  sd(unique_trips))

nrow(dat) # number of location estimates

dat %>%
  group_by(track_id) %>%
  dplyr::summarise(n.uplinks= n()) %>%
  arrange(n.uplinks)  #  number of location estimates per individual

# Extract only the month (month and day)
dat$month_day <- format(dat$date.time, "%m-%d")

dat %>%
  group_by(scientific_name) %>%
  dplyr::summarise(min.month_day = min(month_day),
                   max.month_day = max(month_day))
                   
dat = dat %>% 
  group_by(scientific_name, track_id) %>%
  dplyr::mutate(track.time = max(date.time) - min(date.time)) %>%
  ungroup()
  
dat %>%
  group_by(scientific_name) %>%
  dplyr::summarise(min.track.time = min(track.time),
                   max.track.time = max(track.time),
                   ave.track.time = mean(track.time))

sampling_rate = dat %>% 
  group_by(track_id) %>%
  mutate(time_diff_min = difftime(date.time, lag(date.time), units = "mins"))

mean(sampling_rate$time_diff_min, na.rm = T)
median(sampling_rate$time_diff_min, na.rm = T)

max(dat$Distance)

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

trip_max %>% 
  group_by(scientific_name) %>%
  summarise(mean_max_dist = mean(max_dist),
            sd_max_dist = sd(max_dist))

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
  ylab("Maximum trip distance (km)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_maxdist = g_maxdist + 
  ggbeeswarm::geom_quasirandom(size = 3, width = .15, alpha = .7)  +
  scale_color_manual(values = c("#4daf4c", "#984ea5"), name = "Species") 
  
beeswarm_maxdist

#------------------------
# track_distance
#------------------------
# Reorder factor levels
track_distance$scientific_name_rev <- fct_rev(track_distance$scientific_name)  # Reverses the order of levels

g_trackdist <- ggplot(track_distance, aes(x = scientific_name_rev, 
                          y = trackdistance, 
                          color = scientific_name , fill = scientific_name )) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Cumulative track distance (km)") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_trackdist = g_trackdist + 
  ggbeeswarm::geom_quasirandom(size = 3, width = .15, alpha = .7)  

beeswarm_trackdist

#---------------------------------------
# Fit models for maximum trip distance
#---------------------------------------
# brms models take a while to run!
cores = parallel::detectCores()

hist(trip_max$max_dist, breaks = 20)

# fit a species (group) model
mod_trip_max_spp <- brm(max_dist ~ scientific_name + (1 | track_id), 
                     data = trip_max, 
                     family = lognormal(),
                     control = list(adapt_delta = 0.9),
                     cores = cores,
                     file = "./output/brms/mod_trip_max_spp")

performance::check_predictions(mod_trip_max_spp) 

plot(mod_trip_max_spp)

# model confirmation
loo_logn = loo(mod_trip_max_spp)
loo_logn

#---------------------
# Plot contrasts
#---------------------
# add_epred_draws() adds draws from the expectation of the posterior predictive distribution
# compare_levels compares the value of draws of some variable for different levels of a factor

contrast_trip_epred = trip_max %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name, "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_trip_max_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .epred,  fill = after_stat((x) > 0))) +
  stat_halfeye() +
  labs(x = "Difference in expectation of maximum trip distance (km)",
       y = "Density") +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  gg_theme() + 
  scale_fill_manual(values = c("skyblue", "gray80"))+ 
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.85))

contrast_trip_epred

#add_predicted_draws() adds draws from the posterior predictive distribution 

contrast_trip_predicted = trip_max %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name, "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_predicted_draws(mod_trip_max_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.prediction, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .prediction, fill = after_stat((x) > 0))) +
  stat_halfeye() +
  labs(x = "Difference in posterior predictive distribution | maximum trip distance (km)",
       y = "Density") +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  gg_theme() + 
  xlim(-60,60) + 
  scale_fill_manual(values = c("skyblue", "gray80"))+ 
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.85))

contrast_trip_predicted

# save contrast plots - arrange in a 1x2 layout
ggsave(plot = (contrast_trip_epred | contrast_trip_predicted) + 
         plot_annotation(tag_levels = 'A'), 
       bg = 'white', 
       filename = "./figures/contrasts_trip.png", width=12, height=6)


#------------------------------------------
# plot Posterior predictions across species
#------------------------------------------
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/
# https://www.andrewheiss.com/blog/2022/09/26/guide-visualizing-types-posteriors/
  
grand_mean <- mod_trip_max_spp  %>% 
  epred_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
              re_formula = NA)

grand_mean %>% mean_hdi(.width = 0.9)

plot_grand_mean <- ggplot(data = grand_mean, 
                   aes(x = .epred, y = scientific_name, 
                   fill = scientific_name)) +
  stat_halfeye(slab_alpha = 0.5) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  labs(x = "Maximum trip distance (km) (expectation of the PPD)",  # posterior predictive distribution 
       y = NULL,
       fill = "Species")+ 
  gg_theme() +
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 8.5, y = 1.9, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 12.5, y = 0.9, label = "Northern giant petrel", colour = "black", size = 4) +
  scale_x_continuous(lim= c(2,18)) 

plot_grand_mean 

beeswarm_maxdist / plot_grand_mean 

#------------------------------------
# Combined plot
#------------------------------------
# Adding the elements from the second plot to the first
# Need to switch x and y axis around:

# e_pred
combined_plot_v <- beeswarm_maxdist + stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.3) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")  

# Display the combined plot
print(combined_plot_v)

# posterior_predictions
posterior_predictions <- mod_trip_max_spp  %>% 
        predicted_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
        re_formula = NA)

sgp_pp = posterior_predictions %>%
   filter(scientific_name == "Southern giant petrel")
ngp_pp = posterior_predictions %>%
  filter(scientific_name == "Northern giant petrel")


combined_plot <- beeswarm_maxdist + 
  stat_halfeye(data = posterior_predictions, aes(y = .prediction, x = scientific_name, 
                      fill = scientific_name), slab_alpha = 0.3, scale = 0.6,
     #  point_interval =  mean_hdi, .width = 0.8) +  # definition of circle and whisker in stat_halfeye
  # hdi yields the highest-density interval(s) (also known as the highest posterior density interval)
  interval_alpha = 0, point_interval = NULL) +  # definition of circle and whisker in stat_halfeye
#  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  scale_fill_manual(values = c("grey51", "grey51"), name = "Species") +
  theme(legend.position = "none")   + 
  xlab("") + 
  ggnewscale::new_scale_fill() +
  stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.65, scale = 0.4,
              # interval_alpha = 0, point_interval = NULL) +
              point_color = "black", interval_color = "transparent", size = 9)+
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()  + 
  annotate(geom = "text", x = 0.9, y = 15, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 1.9, y = 15, label = "Northern giant petrel", colour = "black", size = 4) 
  
# Display the combined plot
print(combined_plot)

# # save plot
# pdf("./figures/trip_maxdistance.pdf", width = 5, height = 7, useDingbats = FALSE)
# beeswarm_maxdist / plot_grand_mean 
# dev.off()

# # save plot
# ggsave(plot = combined_plot, bg = 'white', 
#        filename = "./figures/combined_plot_trip.png", width=5,height=4)


#-------------------------------------------
# Fit models for cumulative track distance
#-------------------------------------------
hist(trip_max$max_dist, breaks = 20)

# fit a species (group) model
mod_track_spp <- brm(trackdistance ~ scientific_name + (1 | track_id),
                      data = track_distance, 
                      family = lognormal(),
                      control = list(adapt_delta = 0.9),
                      cores = cores,
                      file = "./output/brms/mod_track_spp")

# performance::check_predictions(mod_track_null) 
performance::check_predictions(mod_track_spp) 

plot(mod_track_spp)

# model confirmation
loo_logn_track = loo(mod_track_spp)
loo_logn_track

# add_epred_draws() adds draws from the expectation of the posterior predictive distribution
# compare_levels compares the value of draws of some variable for different levels of a factor

# Posterior predictions across species
grand_mean_track <- mod_track_spp  %>% 
  epred_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
              re_formula = NA)

plot_grand_mean_track <- ggplot(data = grand_mean_track, 
                                aes(x = .epred, y = scientific_name, 
                                    fill = scientific_name)) +
  stat_halfeye(slab_alpha = 0.5) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  labs(x = "Expected cumulative track distance (km)", 
       y = NULL,
       fill = "Species")+ 
  gg_theme() +
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 21, y = 1.9, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 91, y = 0.9, label = "Northern giant petrel", colour = "black", size = 4) +
  scale_x_continuous(lim= c(0,160)) 

beeswarm_trackdist / plot_grand_mean_track 

# save plot
# pdf("./figures/track_distances.pdf", width = 5, height = 7, useDingbats = FALSE)
# beeswarm_trackdist / plot_grand_mean 
# dev.off()

#---------------------
# Plot contrasts
#---------------------

contrast_track_epred = track_distance %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name,  "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_track_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .epred, fill = after_stat((x) > 0))) +  
  stat_halfeye() +
  labs(x = "Difference in expectation of track distance (km)",
       y = "Density") +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  gg_theme() + 
  scale_fill_manual(values = c( "skyblue", "gray80"))+ 
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.85))

contrast_track_epred

#add_predicted_draws() adds draws from the posterior predictive distribution 

contrast_track_predicted = track_distance %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name,  "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_predicted_draws(mod_track_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.prediction, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .prediction, fill = after_stat((x) > 0))) +
  stat_halfeye() +
  labs(x = "Difference in posterior predictive distribution | track distance (km)",
       y = "Density") +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  gg_theme() + 
  xlim(-200,200) + 
  scale_fill_manual(values = c("skyblue", "gray80")) + 
  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.85))

contrast_track_predicted


# save contrast plots - arrange in a 1x2 layout
ggsave(plot = (contrast_track_epred | contrast_track_predicted) + 
         plot_annotation(tag_levels = 'A'), 
       bg = 'white', 
       filename = "./figures/contrasts_track.png", width=12, height=6)


#------------------------------------
# Combined plot
#------------------------------------
# Adding the elements from the second plot to the first
# Need to switch x and y axis around:

# e_pred
combined_plot_v2 <- beeswarm_trackdist + 
  stat_halfeye(data = grand_mean_track, aes(y = .epred, x = scientific_name, 
                                            fill = scientific_name), slab_alpha = 0.3) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")  

# Display the combined plot
print(combined_plot_v2)

# posterior_predictions
posterior_predictions_track <- mod_track_spp  %>% 
  predicted_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
                  re_formula = NA)

sgp_pp_track = posterior_predictions_track %>%
  filter(scientific_name == "Southern giant petrel")
ngp_pp_track = posterior_predictions_track %>%
  filter(scientific_name == "Northern giant petrel")


combined_plot_track <- beeswarm_trackdist + 
    stat_halfeye(data = posterior_predictions_track, aes(y = .prediction, x = scientific_name, 
                      fill = scientific_name), slab_alpha = 0.3, scale = 0.6,
              #   point_color = "black", interval_color = "black",
                 point_interval =  NULL) +  # definition of circle and whisker in stat_halfeye
     # hdi yields the highest-density interval(s) (also known as the highest posterior density interval)
     #          interval_alpha = 0, point_interval = NULL) +  # definition of circle and whisker in stat_halfeye
#  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  scale_fill_manual(values = c("grey51", "grey51"), name = "Species") +
  theme(legend.position = "none")   + 
  xlab("") + 
  ggnewscale::new_scale_fill() +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.65, scale = 0.4,
               point_color = "black", interval_color = "transparent", size = 9)+
         #      interval_alpha = 0, point_interval = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()  + 
  ylim(0,165) + 
  annotate(geom = "text", x = 0.9, y = 85, label = "Southern giant petrel", colour = "black", size = 4) +
  annotate(geom = "text", x = 1.9, y = 85, label = "Northern giant petrel", colour = "black", size = 4) 

# Display the combined plot
print(combined_plot_track)

combined_plot + combined_plot_track + plot_annotation(tag_levels = "A")

# save plot
ggsave(plot = combined_plot + combined_plot_track + plot_annotation(tag_levels = "A"),
       bg = 'white',
       filename = paste0("./figures/trip_and_track_distances_pp.png"),width=9,height=4.5)


#------------------------------------
# Posterior contrasts HDPI for PREDICTED
#------------------------------------
track_samples = track_distance %>%
  mutate(scientific_name = fct_relevel(scientific_name, "Northern giant petrel", "Southern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_predicted_draws(mod_track_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.prediction, by=scientific_name,comparison = "pairwise")

rethinking::HPDI(track_samples$.prediction, prob = 0.9)

trip_samples = trip_max %>%
  mutate(scientific_name = fct_relevel(scientific_name, "Northern giant petrel", "Southern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_predicted_draws(mod_trip_max_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.prediction, by=scientific_name,comparison = "pairwise")

rethinking::HPDI(trip_samples$.prediction, prob = 0.9)

#------------------------------------
# Posterior contrasts HDPI for EPRED
#------------------------------------
trip_mean_samples = trip_max %>%
  mutate(scientific_name = fct_relevel(scientific_name, "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_trip_max_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise")

rethinking::HPDI(trip_mean_samples$.epred, prob = 0.9)

track_mean_samples = track_distance %>%
  mutate(scientific_name = fct_relevel(scientific_name, "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_track_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise")

rethinking::HPDI(track_mean_samples$.epred, prob = 0.9)

# See Statistical Rethinking: section 3.2
# can calculate the probability that the means differ:
trip_mean_samples %>% 
  count(.epred > 0) %>% 
  mutate(probability = n / sum(n),
         percent = probability * 100)

track_mean_samples %>% 
  count(.epred > 0) %>% 
  mutate(probability = n / sum(n),
         percent = probability * 100)


#--------------------------------
# Directly from the posterior: 
#--------------------------------
# Extract posterior samples
posterior_samples <- as_draws_df(mod_trip_max_spp)
contrast <- posterior_samples$b_scientific_nameSoutherngiantpetrel 

# Calculate probabilities
prob_above_zero <- mean(contrast > 0)  # Proportion of posterior > 0
prob_below_zero <- mean(contrast < 0)  # Proportion of posterior < 0

# Summarize results
cat("Proportion trip distance above zero:", prob_above_zero, "\n")
cat("Proportion trip distance below zero:", prob_below_zero, "\n")


#--------------------------------
# Directly from the posterior: 
#--------------------------------
# Extract posterior samples
posterior_samples <- as_draws_df(mod_track_spp)
contrast <- posterior_samples$b_scientific_nameSoutherngiantpetrel 

# Calculate probabilities
prob_above_zero <- mean(contrast > 0)  # Proportion of posterior > 0
prob_below_zero <- mean(contrast < 0)  # Proportion of posterior < 0

# Summarize results
cat("Proportion track distance above zero:", prob_above_zero, "\n")
cat("Proportion track distance below zero:", prob_below_zero, "\n")


# https://awellis.github.io/learnmultilevelmodels/posts/2021-05-24-walkthrough-brms/

