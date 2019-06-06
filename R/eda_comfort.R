# Exploratory analysis of bike comfort data
# Jane Carlen, May 2019

# Notes
#   Rating distributions are more bimodal than normal (bumps for high and low), and certain qualities (e.g. don't like biking) seem to flatten out the distribution rather than just moving the mean
#   Only three have so much spread that their intercept is not statistically significiant 
#     see lm.video_name -- note one that's not stat. sig. is the reference level -> not shown in summary

# Questions:
#   What explains the tails in the ratings, i.e. people who deviate from the general trend?
#   Do people have low-rating and high-rating tendencies, and are they explained by covariates?
#----------------------------------------------------------------------------------------------
# 0. Setup ----

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

setwd("~/Documents/videosurvey/")

# 1. Load data ----
source("R/data_comfort.R")

# 2. Explore video ratings distributions ----

# There seems to be high-comfort and low-comfort streets

#   Overall ratings distribution ----

png("IMG/ratings_overall_hists.png", width = 800)
par(mfrow = c(1,2))
#mean
hist(d.video$mean_comfort, breaks = 20, xlab = "mean rating", main = "Distribution of mean video ratings")
#median
hist(d.video$median_comfort, breaks = 20, xlab = "median rating", main = "Distribution of median video ratings")
dev.off()

ggplot(d.video) + geom_histogram(aes(x = median_comfort), stat = "count")

#   By video ratings distribution ----
ggplot(d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)), 
       aes(x = comfort_rating, y = fct_reorder(video_name, mean_cr), group = video_name, fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")
ggsave("IMG/ratings_video_dists.png", width = 5, height = 8)

# Which videos have wide variation? (4th_AddisonUniversity, Tunnel_OakRidgeUplands, Ashby_ColbyRegent)
d$video_name = relevel(d$video_name, ref = "4th_AddisonUniversity")
summary(lm(as.numeric(d$comfort_rating) ~ video_name, data = d)) #R^2 .2039

#Video rating distribution by video and gender
ggplot(d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>% filter(!is.na(female)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(female, video_name), mean_cr), 
              group = interaction(female, video_name), fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(2,  alpha = .7), labels= c("male", "female")) +
  xlab("") + ylab("")
ggsave("IMG/ratings_video_gender_dists.png", width = 6, height = 12)

#Video rating distribution by video and "op_like_biking" (3 levels)
ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(op_like_biking_3lev != "neutral" & !is.na(op_like_biking_3lev)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(as.numeric(op_like_biking_3lev)-1, video_name), mean_cr), 
           group = interaction(as.numeric(op_like_biking_3lev), video_name), fill = as.factor(op_like_biking_3lev))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(2,  alpha = .7), labels= c("don't like", "like")) +
  xlab("") + ylab("")
ggsave("IMG/ratings_video_lkbike_dists.png", width = 7, height = 10)

#Video rating distribution by video and "usual_mode =="Bike " (3 levels)
ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(!is.na(usual_mode)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(usual_mode=="Bike", video_name), mean_cr), 
           group = interaction(usual_mode=="Bike", video_name), fill = as.factor(usual_mode=="Bike"))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("") +
  scale_fill_manual(values = rainbow(2,  alpha = .7), labels= c("False", "True"))

# 3. Explore independent variables  ----

# Plot video score against all variables

## Probably important varaibles based on next plots ##

# Composite scores (NCHRP_BLOST_ST, HCM_BLOS_ST, LTS_ST), as expected
# bike lane and it's width/seperation as expressed by buffered / parking lane width / bikeway width / shoulder width / outside land width
# Speed limit and prevailing speed, possibly
# Vehicle volume and pavement condition
# Whether road is divided and urban (these are less intuitive, may be conflation)

d.video.melt = d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
  mutate_if(is.factor, list(as.numeric)) %>%
  mutate(comfort_rating = d$comfort_rating) %>% #actually don't want this one numeric
  select(-c("NCHRP_BLOS_score_ST", "veh_volume_ST")) %>% #remove near duplicats
  melt(id = c("video_name", "comfort_rating")) %>%
  group_by(value, variable, comfort_rating) %>%
  summarize(count = n())

ggplot(d.video.melt, aes(x = reorder(as.factor(value), sort(value)), y = count, fill = as.factor(comfort_rating))) +
  scale_fill_brewer(palette = "RdYlGn") + 
  geom_bar(stat = "identity", position = "dodge", color = "black", lwd = .1) + facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + xlab("")

ggsave("IMG/ratings_by_variable.png", width = 18, height = 9)

# Condensed ratings to negative, positive
d.video.melt3 = d.video.melt %>% mutate(comfort_rating_3lev = as.factor(sapply(as.numeric(comfort_rating), condense_ratings, levels = 7))) %>%
  filter(comfort_rating_3lev != "neutral" & !is.na(comfort_rating_3lev)) %>%
  group_by(comfort_rating_3lev, variable, value) %>% summarize(count = sum(count))

ggplot(d.video.melt3 %>% group_by(variable), aes(x = reorder(as.factor(value), rank(value)), y = count, fill = as.factor(comfort_rating_3lev))) +
  scale_fill_manual(values = rainbow(3, alpha = .7)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black", lwd = .2) +
  facet_wrap(~variable, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + xlab("")

ggsave("IMG/ratings_binary_by_variable.png", width = 18, height = 9)



#    Explore type of effects (additive vs. multiplicative) ----
# Does liking biking imply a shift, or multiplicative?
d %>% group_by(video_name, op_like_biking_3lev) %>%
  summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  filter(op_like_biking_3lev!="neutral") %>%
  ggplot() + 
  geom_path(aes(x = fct_reorder(video_name, m), y = m, color = op_like_biking_3lev, group = video_name))  +
  geom_point(aes(x = fct_reorder(video_name, m), y = m, color = op_like_biking_3lev), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does gender imply a shift, or multiplicative?
d %>% group_by(video_name, female) %>% summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  ggplot() + 
  geom_path(aes(x = fct_reorder(video_name, m), y = m, color = as.factor(female), group = video_name))  +
  geom_point(aes(x = fct_reorder(video_name, m), y = m, color = as.factor(female)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()

# Does bike line have an interaction effect with liking biking? (Seems not)
d %>% group_by(bike_lane_ST, op_like_biking_3lev) %>% 
  summarize(m = mean(as.numeric(comfort_rating), na.rm = T)) %>%
  filter(op_like_biking_3lev!="neutral") %>%
  ggplot() + 
  geom_path(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev), group = bike_lane_ST))  +
  geom_point(aes(x = bike_lane_ST, y = m, color = as.factor(op_like_biking_3lev)), size = 5) +
  scale_color_manual(values = c("red", "green")) + coord_flip()


# 4. Comparing ratings to "scores" ----

d.scores = d %>% select(matches("NCHRP|HCM|LTS|rating|video_name")) %>% mutate_if(is.ordered, as.numeric) 

ggplot(melt(d.scores %>% select(-c("comfort_rating_3lev", "NCHRP_BLOS_score_ST")), id = c("video_name", "comfort_rating")) %>%
         group_by(interaction(value, variable)) %>% mutate(m = mean(comfort_rating))) + 
  geom_density(aes(x = comfort_rating, fill = variable, color = variable), bw = .7) + 
  facet_wrap(~interaction(value, variable), scales = "free_y", ncol = 1)  +
  geom_vline(aes(xintercept = m), col = "red") + 
  theme_bw()

ggsave(filename = "IMG/ratings_vs_scores.png", height = 16)

ggplot(melt(d.scores %>% select(-c("comfort_rating_3lev", "NCHRP_BLOS_score_ST")), id = c("video_name", "comfort_rating")) %>%
         group_by(interaction(value, variable)) %>% mutate(m = mean(comfort_rating))) + 
  geom_density_ridges(aes(x = comfort_rating, y = interaction(value, variable), 
                          fill = variable, color = variable), bandwidth = .7, color = "black") + 
  theme_bw()

ggsave(filename = "IMG/ratings_vs_scores_joy.png", height = 12)
