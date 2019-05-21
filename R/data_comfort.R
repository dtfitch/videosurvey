# Load bike comfort data
# Used by eda_comfort.R for exploratory analysis
# and model_comfort.R for modeling
# Jane Carlen, May 2019

# Data questions for Dillon;
# 1. Why so many blank entries in the child fields? Why a few "Nones" in addition to zeros? (free-entry fields?)
# 2. Coding of hh_composition? Conflicting levels selected. Using rent_share as a hh composition type variable because it has least missing data nad is still somewhat discriptive of household type


# 0. Setup ----

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

setwd("~/Documents/videosurvey/")

  
# 0b. Helper functions

condense_ratings = function(x, levels) {
  if (x %in% floor(1:(levels/2) ) ) return("uncomfortable")
  else if (x %in% (levels+1)/2) return("neither")
  else if (x %in% ceiling((levels+2)/2):levels) return("comfortable")
  else return(NA)
}

# 1. Load Data ----

d <- readRDS("video_survey_data_long.RDS") 
d.meta <- read.csv("Metadata_video_survey_data_long.csv")

# make street variables clear from names so easy to select
names(d)[(which(names(d) == "video_name")+1):ncol(d)] = paste0(names(d)[(which(names(d) == "video_name")+1):ncol(d)], "_ST")

# remove some redunant vas
d <- d %>% select(-street_divided_ST)

# Add some 3-level vars
d <- d %>% 
  mutate(op_like_biking_3lev = factor(unlist(sapply(as.numeric(op_like_biking), condense_ratings, levels = 5)), 
                                      levels = c("uncomfortable", "comfortable"), ordered = TRUE)) %>%
  mutate(comfort_rating_3lev = as.ordered( 
    factor( sapply(as.numeric(comfort_rating), condense_ratings, levels = 7) , levels = c("uncomfortable", "comfortable") )
  ))

# how many levels in each of the street vars?
sapply(d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
         mutate_at(vars(contains("_ST")), as.factor), nlevels)


summary(d)

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

#   Quick checks on representativeness of sample ----
# Note it's twice as female than male, and mostly undergrad
table(d$female)
table(d$primary_role)
# 2. Watch videos
d %>% group_by(video_name) %>% summarize(first(URL))

# videos with wider variation in seem to have a mix of good and bad features,
# e.g. slower residential street but no bike lane, faster street with shoulder/bike lane
# wider, slower street with no bike lane