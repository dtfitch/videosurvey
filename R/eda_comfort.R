# Exploratory analysis of bike comfort data
# Jane Carlen, May 2019

# Videos generally have a positive or negative comfort rating, not many with rating evenly spread or concentrated on middle rating

# Questions:
#   What explains the tails in the ratings, i.e. people who deviate from the general trend
#   Do people have low-rating and high-rating tendencies, and are they explained by covariates?
#   I notice all score distributions are more bimodal than normal (bumps for high and low), and individual qualities seem to flatten out the distribution rather than moving the mean.

# 0. Setup ----

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

setwd("~/Documents/videosurvey/")

# load data
source("R/data_comfort.R")

# 2. Explore video ratings distributions ----

# There seems to be high-comfort and low-comfort streets

#Overall ratings distribution
hist(d.video$mean_comfort, breaks = 20)
hist(d.video$median_comfort, breaks = 20)

#Video rating distribution by video
ggplot(d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)), 
       aes(x = comfort_rating, y = fct_reorder(video_name, mean_cr), group = video_name, fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Which videos have wide variation? (4th_AddisonUniversity, Tunnel_OakRidgeUplands, Ashby_ColbyRegent)
d$video_name = relevel(d$video_name, ref = "4th_AddisonUniversity")
summary(lm(as.numeric(d$comfort_rating) ~ video_name, data = d)) #R^2 .2039

#Video rating distribution by video and gender
ggplot(d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(female, video_name), mean_cr), 
              group = interaction(female, video_name), fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Video rating distribution by video and "op_like_biking" (3 levels)
ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(op_like_biking_3lev != "neither" & !is.na(op_like_biking_3lev)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(op_like_biking_3lev, video_name), mean_cr), 
           group = interaction(op_like_biking_3lev, video_name), fill = as.factor(op_like_biking_3lev))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("limegreen", "red2"), labels= c("like", "dont_like"))

#Video rating distribution by video and "usual_mode =="Bike " (3 levels)
ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(!is.na(usual_mode)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(usual_mode=="Bus", video_name), mean_cr), 
           group = interaction(usual_mode=="Bus", video_name), fill = as.factor(usual_mode=="Bus"))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("limegreen", "red2"), labels= c("false", "true"))

# Does liking biking imply a shift, or multiplicative?
d %>% group_by(video_name, op_like_biking_3lev) %>% summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = video_name, y = m, color = op_like_biking_3lev, group = video_name))  +
  geom_point(aes(x = video_name, y = m, color = op_like_biking_3lev), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does liking biking imply a shift, or multiplicative?
d %>% group_by(video_name, female) %>% summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = video_name, y = m, color = as.factor(female), group = video_name))  +
  geom_point(aes(x = video_name, y = m, color = as.factor(female)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does liking biking imply a shift, or multiplicative?
d %>% group_by(bike_lane_ST, op_like_biking_3lev) %>% summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev), group = bike_lane_ST))  +
  geom_point(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# ratings distributions
ggplot(d) + geom_histogram(aes(x = comfort_rating), stat = "count")
ggplot(d.video) + geom_histogram(aes(x = median_comfort), stat = "count")

# 3. Relationship between video score and variables ----

# Indicates that bike lane, NCHRP_BLOST_ST, HCM_BLOS_ST, LTS_ST, buffer, bikeway width 

# Plot video score against all variables
ggplot(d.video.melt, aes(x = reorder(as.factor(value), sort(value)), y = count, fill = as.factor(comfort_rating))) +
  scale_fill_brewer(palette = "RdYlGn") + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

  # Condensed ratings to uncomfortable, comfortable
  d.video.melt3 = d.video.melt %>% mutate(comfort_rating_3lev = as.factor(sapply(comfort_rating, condense_ratings, levels = 7))) %>%
    filter(comfort_rating_3lev != "neither" & !is.na(comfort_rating_3lev)) %>%
    group_by(comfort_rating_3lev, variable, value) %>% summarize(count = sum(count))

  ggplot(d.video.melt3 %>% group_by(variable), aes(x = reorder(as.factor(value), rank(value)), y = count, fill = as.factor(comfort_rating_3lev))) +
    scale_fill_manual(values = c("limegreen", "red2")) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~variable, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()


