# Exploratory analysis of bike comfort data
# Jane Carlen, May 2019

# Videos generally have a positive or negative comfort rating, not many with rating evenly spread or concentrated on middle rating
# Questions:
#   What explains the tails in the ratings, i.e. people who deviate from the general trend
#   Do people have low-rating and high-rating tendencies, and are they explained by covariates?
#   I notice all score distributions are more bimodal than normal (bumps for high and low), and individual qualities seem to flatten out the distribution rather than moving the mean.
# 0. Setup 

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

setwd("~/Documents/bike_its/")

# 0b. Helper functions

condense_ratings = function(x, levels) {
  if (x %in% floor(1:(levels/2) ) ) return("uncomfortable")
  else if (x %in% (levels+1)/2) return("neither")
  else if (x %in% ceiling((levels+2)/2):levels) return("comfortable")
  else return(NA)
}

# 1. Load Data

d <- readRDS("video_survey_data_long.RDS")
d.meta <- read.csv("Metadata_video_survey_data_long.csv")

# make street variables clear from names so easy to select
names(d)[(which(names(d) == "video_name")+1):ncol(d)] = paste0(names(d)[(which(names(d) == "video_name")+1):ncol(d)], "_ST")

# how many levels in each of the street vars?
sapply(d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
        mutate_at(vars(contains("_ST")), as.factor), nlevels)


summary(d)

d <- d %>% mutate(op_like_biking3 = as.factor(unlist(sapply(as.numeric(op_like_biking), condense_ratings, levels = 5)))  )

d.video = d %>% group_by(video_name) %>% summarize(
  mean_comfort = mean(as.numeric(comfort_rating), na.rm = T),
  median_comfort = median(as.numeric(comfort_rating), na.rm = T),
  speed = first(bike_speed_mph_ST),
  NCHRP_BLOS_score = first(NCHRP_BLOS_score_ST)
  )

d.video.melt = d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
                  mutate_if(is.factor, list(as.numeric)) %>%
                  melt(id = c("video_name", "comfort_rating")) %>%
                  group_by(value, variable, comfort_rating) %>%
                  summarize(count = n())

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

#Video rating distribution by video and gender
ggplot(d %>% group_by(video_name) %>% mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(female, video_name), mean_cr), 
              group = interaction(female, video_name), fill = as.factor(female))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Video rating distribution by video and "op_like_biking" (3 levels)
ggplot(d %>% group_by(video_name) %>% 
         mutate(mean_cr = mean(as.numeric(comfort_rating), na.rm  = T)) %>%
         filter(op_like_biking3 != "neither" & !is.na(op_like_biking3)), 
       aes(x = comfort_rating, y = fct_reorder(interaction(op_like_biking3, video_name), mean_cr), 
           group = interaction(op_like_biking3, video_name), fill = as.factor(op_like_biking3))) +
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("limegreen", "red2"), labels= c("like", "dont_like"))

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
  d.video.melt3 = d.video.melt %>% mutate(comfort_rating3 = as.factor(sapply(comfort_rating, condense_ratings, levels = 7))) %>%
    filter(comfort_rating3 != "neither" & !is.na(comfort_rating3)) %>%
    group_by(comfort_rating3, variable, value) %>% summarize(count = sum(count))

  ggplot(d.video.melt3 %>% group_by(variable), aes(x = reorder(as.factor(value), rank(value)), y = count, fill = as.factor(comfort_rating3))) +
    scale_fill_manual(values = c("limegreen", "red2")) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~variable, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_minimal()


