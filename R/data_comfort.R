# Load bike comfort data
# Used by eda_comfort.R for exploratory analysis
# and model_comfort.R for modeling
# Jane Carlen, May 2019

# Data questions for Dillon;
# 1. Why so many blank entries in the child fields? Why a few "Nones" in addition to zeros? (free-entry fields?)
# 2. Coding of hh_composition? Conflicting levels selected. Using rent_share as a hh composition type variable because it has least missing data nad is still somewhat discriptive of household type
# 3. Are blocked bike lanes really blocked? (https://youtu.be/LI0m8h3jVJ4, https://youtu.be/XAKiJ78Z8uE)

# 0. Setup ----

library(ggplot2)
library(ggridges)
library(dplyr)
library(forcats)
library(reshape2)

setwd("~/Documents/videosurvey/")

  
#   0b. Helper functions ----

condense_ratings = function(x, levels) {
  if (x %in% floor(1:(levels/2) ) ) return("negative")
  else if (x %in% ((levels+1)/2)) return("neutral")
  else if (x %in% ceiling((levels+2)/2):levels) return("positive")
  else return(NA)
}

# 1. Load Data ----

d <- readRDS("video_survey_data_long.RDS") 
d.block <-read.csv("VideoBlocks_LUT.csv", stringsAsFactors = F)
# Variables added to the data later (bike boulevard is for internal tracking, won't be used):
d = left_join(d, d.block[,c("ID", "bike_operating_space", "bike_boulevard")], by = "ID") 
d.meta <- read.csv("Metadata_video_survey_data_long.csv")

# 2. Some Variable Clean + Transformation
#   Make street variables clear from names so easy to select ----
names(d)[(which(names(d) == "video_name")+1):ncol(d)] = paste0(names(d)[(which(names(d) == "video_name")+1):ncol(d)], "_ST")

#   Remove some redunant vars ----
d <- d %>% select(-street_divided_ST)

#   Reduce factors for some variables ----
d <- d %>% 
  mutate(op_like_biking_3lev = factor(unlist(sapply(as.numeric(op_like_biking), condense_ratings, levels = 5)), 
                                      levels = c("negative", "neutral", "positive"), ordered = TRUE)) %>%
  mutate(comfort_rating_3lev = as.ordered( 
    factor( sapply(as.numeric(comfort_rating), condense_ratings, levels = 7) , levels = c("negative", "neutral", "positive") )
  ))

# how many levels in each of the street vars?
sapply(d %>% select(c("comfort_rating", "video_name", contains("_ST", ignore.case = FALSE))) %>%
         mutate_at(vars(contains("_ST")), as.factor), nlevels)

d$usual_mode_4lev = d$usual_mode
levels(d$usual_mode_4lev) = c(NA, "Bike", "Public Trans", "Car", "Car", "Bike", "Car", "Other", "Other", "Other", "Car", "Public Trans", "Car", "Other")
# Other includes motorcycle or scooter, other, skate or skateboard, and walk

d$secondary_mode_BIKE = d$secondary_mode
levels(d$secondary_mode_BIKE) = c(NA, "Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Not_Bike")

d$hh_composition_4lev = d$hh_composition
levels(d$hh_composition_4lev) = c(NA, "Alone only", "Multi", "Multi", "Multi", "Family only", "Roommates etc. only", "Multi")

#25, 30 & 35, 45 & 50
d$speed_limit_mph_ST_3lev = cut(d$speed_limit_mph_ST, breaks = c(25, 30, 40, 50), include.lowest = TRUE, right = FALSE) 

#   Convert some factors to ordered ----

# List unordered factors 
names(d)[sapply(d, is.factor) & !sapply(d, is.ordered)]

d$rent_share = ordered(d$rent_share, levels = levels(d$rent_share)[c(1,4,2,3)])
levels(d$rent_share)[1] = NA

d$HCM_BLOS_ST = ordered(d$HCM_BLOS_ST, levels = rev(levels(d$HCM_BLOS_ST))) #Ordered alphabetically, best is A

d$NCHRP_BLOS_ST = ordered(d$NCHRP_BLOS_ST, levels = rev(levels(d$NCHRP_BLOS_ST))) #Ordered alphabetically, best is A

d$veh_volume_ST = ordered(d$veh_volume_ST, levels = rev(levels(d$veh_volume_ST))) 

d$veh_volume2_ST = ordered(d$veh_volume2_ST, levels = rev(levels(d$veh_volume2_ST))) 

#   Replace "" with NA ----
sort(colSums(apply(d, 2, "==", ""), na.rm = T))
levels(d$rent_split)[1] = NA
levels(d$housing_unit_type)[1]=NA
levels(d$hh18_older)[1]=NA
levels(d$hh_composition)[1]=NA
levels(d$edu_self)[1]=NA
levels(d$edu_parent)[1]=NA
levels(d$license)[1]=NA
levels(d$secondary_mode)[1]=NA
levels(d$usual_mode)[1]=NA

# 2. Make summary data sets ----

d.video = d %>% group_by(video_name) %>% summarize(
  mean_comfort = mean(as.numeric(comfort_rating), na.rm = T),
  median_comfort = median(as.numeric(comfort_rating), na.rm = T),
  var = var(as.numeric(comfort_rating)),
  speed = first(bike_speed_mph_ST),
  NCHRP_BLOS_ST = first(NCHRP_BLOS_ST),
  block_ID = first(block_ID),
  group = first(VideoGroup)
)

#    Quick checks on representativeness of sample ----
# Note it's twice as female than male, and mostly undergrad
table(d$female)
table(d$primary_role)
# 3. Watch videos ----
d %>% group_by(video_name) %>% summarize(first(URL))

# videos with wider variation in seem to have a mix of good and bad features,
# e.g. slower residential street but no bike lane, faster street with shoulder/bike lane
# wider, slower street with no bike lane

