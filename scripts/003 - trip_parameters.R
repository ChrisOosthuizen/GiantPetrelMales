# Giant petrels - Utilization distributions

## Original script by Ryan Reisinger
## Amended by Chris Oosthuizen

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
  ylab("Maximum trip distance") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_maxdist = g_maxdist + 
  # stat_summary(
  #   fun = median, geom = "point", 
  #   shape = 95, size = 20) +
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
#  scale_y_continuous(lim = c(0,25)) +
  scale_colour_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  ylab("Cumulative track distance") + 
  xlab("Species") + 
  gg_theme() +
  theme(legend.position = "none") 

beeswarm_trackdist = g_trackdist + 
  # stat_summary(
  #   fun = median, geom = "point", 
  #   shape = 95, size = 20) +
  ggbeeswarm::geom_quasirandom(size = 3, width = .15, alpha = .7)  

beeswarm_trackdist

#---------------------------------------
# Fit models for maximum trip distance
#---------------------------------------
# brms models take a while to run!
cores = parallel::detectCores()

hist(trip_max$max_dist, breaks = 20)

# fit a null model
mod_trip_max_null <- brm(max_dist ~ 1 + (1 | track_id),
                     data = trip_max, 
                     family = lognormal(),
                     control = list(adapt_delta = 0.9),
                     cores = cores,
                     file = "./output/brms/mod_trip_max_null")

# fit a species (group) model
mod_trip_max_spp <- brm(max_dist ~ scientific_name + (1 | track_id), 
                     data = trip_max, 
                     family = lognormal(),
                     control = list(adapt_delta = 0.9),
                     cores = cores,
                     file = "./output/brms/mod_trip_max_spp")

performance::check_predictions(mod_trip_max_null) 
performance::check_predictions(mod_trip_max_spp) 

# model comparison / confirmation
loo_null = loo(mod_trip_max_null)
loo_logn = loo(mod_trip_max_spp)

model_comp = loo_compare(loo_null, loo_logn)
print(model_comp, simplify = FALSE, digits = 2)

# https://www.andrewheiss.com/blog/2023/05/15/fancy-bayes-diffs-props/

trip_max %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name,             
      "Northern giant petrel", "Southern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_trip_max_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = "blue4") +
  labs(x = "Percentage point difference in proportions",
       y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

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
  labs(x = "Maximum trip distance (expectation of the posterior predictive distribution)", 
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


#------------------------------------
# Combined plot
#------------------------------------
# Adding the elements from the second plot to the first
# Need to switch x and y axis around:

# e_pred
combined_plot <- beeswarm_maxdist + stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.3) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")  

# Display the combined plot
print(combined_plot)

# posterior_predictions
posterior_predictions <- mod_trip_max_spp  %>% 
        predicted_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
        re_formula = NA)

sgp_pp = posterior_predictions %>%
   filter(scientific_name == "Southern giant petrel")
ngp_pp = posterior_predictions %>%
  filter(scientific_name == "Northern giant petrel")

#round(rethinking::PI(sgp_pp$.prediction, p = 0.9),2)
#round(rethinking::PI(ngp_pp$.prediction, p = 0.9),2)

combined_plot <- beeswarm_maxdist + 
  stat_halfeye(data = posterior_predictions, aes(y = .prediction, x = scientific_name, 
                      fill = scientific_name), slab_alpha = 0.2, scale = 0.6,
     #  point_interval =  mean_hdi, .width = 0.8) +  # definition of circle and whisker in stat_halfeye
  # hdi yields the highest-density interval(s) (also known as the highest posterior density interval)
  interval_alpha = 0, point_interval = NULL) +  # definition of circle and whisker in stat_halfeye
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")   + 
  xlab("") + 
  ggnewscale::new_scale_fill() +
  stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.65, scale = 0.1,
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

#-------------------------------------------
# Fit models for cumulative track distance
#-------------------------------------------
hist(trip_max$max_dist, breaks = 20)

# fit a null model
mod_track_null <- brm(trackdistance ~ 1 + (1 | track_id),
                         data = track_distance, 
                         family = lognormal(),
                         control = list(adapt_delta = 0.9),
                         cores = cores,
                         file = "./output/brms/mod_track_null")

# fit a species (group) model
mod_track_spp <- brm(trackdistance ~ scientific_name + (1 | track_id),
                      data = track_distance, 
                      family = lognormal(),
                      control = list(adapt_delta = 0.9),
                      cores = cores,
                      file = "./output/brms/mod_track_spp")

performance::check_predictions(mod_track_null) 
performance::check_predictions(mod_track_spp) 

# model comparison / confirmation
loo_null_track = loo(mod_track_null)
loo_logn_track = loo(mod_track_spp)

model_comp_track = loo_compare(loo_null_track, loo_logn_track)
print(model_comp_track, simplify = FALSE, digits = 2)

track_distance %>%
  # compare_levels() subtracts things using alphabetical order, so we have to
  # change the levels for a 'postive' difference in this case
  mutate(scientific_name = fct_relevel(scientific_name,             
                                       "Southern giant petrel", "Northern giant petrel")) %>% 
  modelr::data_grid(scientific_name) %>%
  add_epred_draws(mod_track_spp, re_formula=NA, allow_new_levels=TRUE) %>%
  compare_levels(.epred, by=scientific_name,comparison = "pairwise") %>%
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = "blue4") +
  labs(x = "Percentage point difference in proportions",
       y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

# Posterior predictions across species
grand_mean_track <- mod_track_spp  %>% 
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

beeswarm_trackdist / plot_grand_mean_track 

# save plot
pdf("./figures/track_distances.pdf", width = 5, height = 7, useDingbats = FALSE)
beeswarm_trackdist / plot_grand_mean 
dev.off()


#------------------------------------
# Combined plot
#------------------------------------
# Adding the elements from the second plot to the first
# Need to switch x and y axis around:

# e_pred
combined_plot <- beeswarm_trackdist + 
  stat_halfeye(data = grand_mean_track, aes(y = .epred, x = scientific_name, 
                                            fill = scientific_name), slab_alpha = 0.3) +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")  

# Display the combined plot
print(combined_plot)

# posterior_predictions
posterior_predictions_track <- mod_track_spp  %>% 
  predicted_draws(newdata = expand_grid(scientific_name = c("Southern giant petrel", "Northern giant petrel")), 
                  re_formula = NA)

sgp_pp_track = posterior_predictions_track %>%
  filter(scientific_name == "Southern giant petrel")
ngp_pp_track = posterior_predictions_track %>%
  filter(scientific_name == "Northern giant petrel")

#round(rethinking::PI(sgp_pp_track$.prediction, p = 0.9),2)
#round(rethinking::PI(ngp_pp_track$.prediction, p = 0.9),2)

combined_plot_track <- beeswarm_trackdist + 
    stat_halfeye(data = posterior_predictions_track, aes(y = .prediction, x = scientific_name, 
                      fill = scientific_name), slab_alpha = 0.2, scale = 0.6,
              #   point_color = "black", interval_color = "black",
                 point_interval =  NULL) +  # definition of circle and whisker in stat_halfeye
     # hdi yields the highest-density interval(s) (also known as the highest posterior density interval)
     #          interval_alpha = 0, point_interval = NULL) +  # definition of circle and whisker in stat_halfeye
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  theme(legend.position = "none")   + 
  xlab("") + 
  ggnewscale::new_scale_fill() +
  scale_fill_manual(values = c("#4daf4c", "#984ea5"), name = "Species") +
  stat_halfeye(data = grand_mean, aes(y = .epred, x = scientific_name, 
                                      fill = scientific_name), slab_alpha = 0.65, scale = 0.1,
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
       filename = paste0("./figures/trip_and_track_distances_pp.png"),width=10,height=6)


#---------------------------------
# This gives the same as above:
#---------------------------------
pred <- marginaleffects::predictions(mod_trip_max_spp,
                                     type = "response", # brms::posterior_epred function.
                                     # type = "prediction", # brms::posterior_predict
                                     newdata = marginaleffects::datagrid(scientific_name = c("Southern giant petrel", 
                                                                                             "Northern giant petrel")),
                                     re_formula = NA)

# The posterior_draws function samples from the posterior distribution of the model, 
# and produces a data frame with drawid and draw columns

pred <- marginaleffects::posterior_draws(pred)

# This “long” format makes it easy to plots results:

ggplot(pred, aes(x = draw, fill = factor(scientific_name))) +
  stat_halfeye(alpha = 0.5) 
#  facet_grid(~ scientific_name, labeller = label_both) 

marginaleffects::avg_slopes(mod_trip_max_spp, conf_level = 0.9)
