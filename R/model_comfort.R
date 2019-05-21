#
# Project: Video Bike Comfort Survey
# Script: model_comfort.R

# Purpose: build models of comfort data
# We want to use these models to understand what makes for a comfortable bicycling environment.
# We want to explore different population segments, consider "types" of bicyclists, and also
# potential environmental thresholds for where the vast majority of this sample would consider
# it comfortable to bicycle.

# Models:
# (1) start with linear models (7 pt likert treated as continuous)
# (2) simple ordinal models (7 pt likert)
# (3) multilevel linear/ordinal models (varying by person, varying by video)

# Issues:
# dimension reduction of video attributes
# 
# Output: TBD
# Author: Dillon Fitch
# Last Edited: Spring 2019


#================
# LOAD LIBRARIES
# ===============
# Below libraries are required to run this script
library(brms)

# ==========
# FUNCTIONS
# ==========
# Below functions are used in the script

# ==========
# MAIN CODE
# ==========
# Set working directory (wherever script and data resids)
# set root path based on home or work computer
if(Sys.info()["nodename"] %in% c("DILLONT440S","DILLON-RYZEN")){
  path.root <- "C:/Users/Dillon/Box Sync/HandyLab/Video_CTS/"
}else{
  path.root <- "C:/Users/dtfitch/Box Sync/HandyLab/Video_CTS/"
}
setwd(paste(path.root,"Analysis",sep=""))

# load cleaned data with no IDs (data)
d <- readRDS("video_survey_data_long.RDS")

# data transformations
d$bike_commuter <- ifelse(d$usual_mode %in% c("Bike","Electric bike"),1,0)
d$bike_very_confident <- ifelse(d$bike_ability =="I am very confident riding a bike",1,0)
d$comfort_rating_numeric <- as.numeric(d$comfort_rating)

# fields for subsetting
fields <- c("comfort_rating","comfort_rating_numeric","person_ID","video_name","female","bike_ability",
            "bike_commuter","op_like_biking")
d.model <- d[fields]

# modelling ideas --------------
# normal or ordinal logit?
fit0 <- brm(comfort_rating_numeric ~ (1|person_ID) + (1|video_name) , 
            data=d.model, family=gaussian(),
            chains=3,cores=3,iter=2000,warmup=1000)
fit1 <- brm(comfort_rating ~ (1|person_ID) + (1|video_name) , 
            data=d.model, family=cumulative(link="logit", threshold="flexible"),
            chains=3,cores=3,iter=2000,warmup=1000)

# Some key individual variables
fit2 <- brm(comfort_rating ~ (1|person_ID) + (1|video_name) + female + bike_very_confident +
              bike_commuter + op_like_biking, 
            data=d, family=cumulative(link="logit", threshold="flexible"),
            chains=3,cores=3,iter=2000,warmup=1000,
            control = list(max_treedepth=12))




            
        