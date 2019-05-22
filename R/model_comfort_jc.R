# Models of bike comfort data
# Jane Carlen, May 2019
#
# To do:
# - random effects in ordered logit
# - decision trees to explore interaction effects
#
# Levels: L, Q, C, 4 ??
#
# -----------------------------------------------------------------------------------
# 0. Setup ####

library(MASS)
try({detach("package:dplyr")})
library(dplyr) # to use dplyr select
library("glmnetcr")

setwd("~/Documents/videosurvey")
source("R/data_comfort.R")

summary(d)
table(d$video_name)
#View(d.meta)

# -----------------------------------------------------------------------------------
# 1. Correlations ----
#   Between opinion variables ----

d.opinion = d %>% group_by(person_ID) %>% select(matches('op|distance')) %>% 
  select(-c("op_like_biking_3lev")) %>% summarise_all(first) %>%
  mutate_all(as.numeric) %>% select(-c("person_ID"))

names(d.opinion)
opinion.cor = cor(d.opinion, use = "pairwise")
round(opinion.cor, 2)
diag(opinion.cor) = 0
round(apply(opinion.cor, 1, max), 2)
# What's correlated with what? - profiles of people satisfied and unsatisfied with commute
# Like biking and eco vs. wanting a car
cbind(rownames(opinion.cor)[round(apply(opinion.cor, 1, which.max), 2)],
      round(apply(opinion.cor, 1, max), 2), 
      rownames(opinion.cor)[round(apply(opinion.cor, 1, which.min), 2)],
      round(apply(opinion.cor, 1, min), 2))

# bug only works with formula input to prcomp (https://stackoverflow.com/questions/12078291/r-function-prcomp-fails-with-nas-values-even-though-nas-are-allowed)

opinion.pca = prcomp(~., center = TRUE, scale. = TRUE, na.action = na.omit, data = d.opinion)
summary(opinion.pca)
# See which variables overlap:
sort(round(opinion.pca$rotation[,1], 2))
sort(round(opinion.pca$rotation[,2], 2))
# picture emerges:
# (1) don't like commute (travel stress, travel wasted, need car, bad schedule) vs.
    #negative on: like commute, commute positive feelints, commute goes well, no change
# (2) limit driving, bike often, eco concern, like biking, like transit vs. 
#   #negative on: like driving, need car, need own car, schedule makes it hard to commute
d %>% select(contains("commute")) %>% mutate_all(as.numeric) %>% cor(use = "pairwise")
table(d$op_commute_positive_negative, d$op_commute_positive_feelings) # a few strange results here 

#   Between demographic variables ----
# Implies to use either education level or primary role (probably primary role, more interpretable)
# Strong correlation between numbers of childen

d.demo = d %>% group_by(person_ID) %>% select(-contains("op")) %>% 
  select(-contains("_ST", ignore.case = FALSE)) %>% 
  select(-contains("ID", ignore.case = FALSE)) %>%
  select(-contains("video", ignore.case = TRUE)) %>%
  summarise_all(first) %>%
  select(-c("URL", "comfort_rating_3lev", "person_ID")) %>%
  # second-round removals
  select(-c("edu_self", "child6", "child615", "child1617", "monthly_housing_cost", "hh18_older"))

d.demo.model.matrix = model.matrix(comfort_rating ~ . -1, data = d.demo)

demo.cor = cor(d.demo.model.matrix, use = "pairwise")
round(demo.cor, 2)
diag(demo.cor) = 0
round(apply(demo.cor, 1, max, na.rm = TRUE), 2)
# What's correlated with what? - profiles of people satisfied and unsatisfied with commute
# Like biking and eco vs. wanting a car
cbind(rownames(demo.cor)[round(apply(demo.cor, 1, which.max), 2)],
      round(apply(demo.cor, 1, max, na.rm = TRUE), 2), 
      rownames(demo.cor)[round(apply(demo.cor, 1, which.min), 2)],
      round(apply(demo.cor, 1, min, na.rm = TRUE), 2))
View(.Last.value)

data.frame(d.demo.model.matrix) %>% select(contains("child")) %>% cor() %>% round(1)

#Exploratory
prop.table(table(d$bike_ability, d$usual_mode_4lev), margin = 1)

#   Between street variables ----

d.street = d %>% group_by(video_name) %>% select(contains("ST", ignore.case = FALSE)) %>% 
  #select(-c("comfort_rating_3lev")) %>% 
  summarise_all(first) %>%
  mutate_all(as.numeric) %>% select(-c("video_name"))

street.cor = cor(d.street, use = "pairwise")
round(street.cor, 2)
diag(street.cor) = 0
round(apply(street.cor, 1, max), 2)
round(apply(street.cor, 1, min), 2)
# What's correlated with what? - profiles of people satisfied and unsatisfied with commute
# Like biking and eco vs. wanting a car
cbind(rownames(street.cor)[round(apply(street.cor, 1, which.max), 2)],
      round(apply(street.cor, 1, max), 2), 
      rownames(street.cor)[round(apply(street.cor, 1, which.min), 2)],
      round(apply(street.cor, 1, min), 2))

# -----------------------------------------------------------------------------------
# 2. Transforming variables -----

d$bike_access = as.factor(d$bike_access)
names(d)[sapply(d, is.factor)]

#   Reduce factors for some variables ----

d$usual_mode_4lev = d$usual_mode
levels(d$usual_mode_4lev) = c("Bike", "Public Trans", "Car", "Car", "Bike", "Car", "Other", "Other", "Other", "Car", "Public Trans", "Car", "Other")
# Other includes motorcycle or scooter, other, skate or skateboard, and walk

d$secondary_mode_BIKE = d$secondary_mode
levels(d$secondary_mode_BIKE) = c("Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Not_Bike", "Not_Bike")

# May want to reduce child fields to binary, but note alot of missing data in these fields
levels(d$child1617) = c(NA,0,1,2,0)
levels(d$child6) = c(NA,0,1,2,3,0)
levels(d$child615) = c(NA,0,1,2,3,4,0)
table(as.numeric(as.character(d$child1617)), useNA = "always")

d$child6 = as.numeric(as.character(d$child6))
d$child615 = as.numeric(as.character(d$child615))
d$child1617 = as.numeric(as.character(d$child1617))

# [1] "Bike"                                                                               
# [2] "Bus"                                                                                
# [3] "Carpool or vanpool with others also going to campus (either as driver or passenger)"
# [4] "Drive alone in a car (or other vehicle)"                                            
# [5] "Electric bike"                                                                      
# [6] "Get a ride (someone drops you off and continues on elsewhere)"                      
# [7] "Motorcycle or scooter"                                                              
# [8] "Other:"                                                                             
# [9] "Skate or skateboard"                                                                
# [10] "Taxi services"                                                                      
# [11] "Train or light rail"                                                                
# [12] "Uber or Lyft Services"                                                              
# [13] "Walk"  

#   Combine vars ----

# Saw that there are several overlapping variables describing feelings about commute, so I summed them
d$op_LIKE_COMMUTE_SUM = rowSums(d %>% select(contains("commute")) %>% mutate_all(as.numeric))

# Protected bike lane implies buffered implies bike lane
# Only one street has a proected bike lane so I didn't make it it's own level
d$bike_lane_SUM_ST = as.factor(d$bike_lane_ST + d$buffer_ST)
#how many streets of each type?
d %>% group_by(video_name) %>% summarize_all(first) %>% select(matches("lane|protected|buffer|blocked")) %>% 
  mutate_all(as.factor) %>% summary()

d$speed_prevail_minus_limit_ST = d$prevailing_speed_mph_ST - d$speed_limit_mph_ST

#   Make new d.model for modeling ----

d.model = d %>% select(-c("cts_ID", "ID", "URL",
                          "block_ID", "sub_ID", #<- might want these later
                          "person_ID", "VideoGroup", "video_name")) %>%
  # hh18_older has weird entries and overlaps with household type anyway.
  # hh_composition is also weirdly coded
  # housing_unit_type also has lots of missing data   
  # rent_split is similar to rent_share but more missing data
  # keep rent share (more descriptive with less missing data)
  # edu_self has too-high correlation with primary role. edu_parent has lots of missing data
  select(-c("hh18_older", "hh_composition", "housing_unit_type",
            "rent_split", 
            "edu_self", "edu_parent")) %>%
  # Only two blocked ones and I looked at the videos -- neither actually seems "blocked"
  select(-c("bike_lane_blocked_ST")) %>%
  # Removed in favor of other version or similar var
  select(-contains("_3lev", ignore.case = FALSE)) %>%
  select(-c("monthly_housing_cost", "veh_volume_ST", #<- used veh_volume2_ST instead, more precise
            "NCHRP_BLOS_ST", "HCM_BLOS_ST",
            "usual_mode", "prevailing_speed_mph_ST", #<- used prevail - speed instead to reduce correlation
            #in bike_lane_SUM_ST
            "bike_lane_ST", "protected_ST", "buffer_ST", 
            # tried just secondary mode is BIKE instead
            "secondary_mode")) %>%
  #removecomposite street vars while examining street factors
  select(-c("NCHRP_BLOS_score_ST", "LTS_ST")) %>%
  # Summed variables describing liking commute instead
  select(-contains("commute", ignore.case = F)) %>%
  # Too many levels (may add vack in later)
  # Reduce levels of seconary mode?
  # Reduce levels of child vars to just have or don't have kid? (lots of missing data tho)
  select(-c("child6", "child615", "child1617")) %>%
  # unlikely relationship (may add back in later)
  select(-c("distance", "op_smartphone", "license")) %>%
  # removed after preliminary models and no strong reason to keep them (may add vack in later)
  select(-c(#"divided_road_ST",
            "op_eco_concern", "bike_access", "op_travel_wasted", "op_like_driving",
            "op_schedule_transit", "op_limit_driving", "op_need_own_car",
            "op_LIKE_COMMUTE_SUM", "secondary_mode_BIKE",
            "op_bike_often", # mostly captured by bike commuting
            "rent_share",
            "urban_ST",
            #strongly correlated with bike_lane_SUM_ST:
            "shoulder_width_ft_ST", #removing this instead of "bike_lane_and_parking_lane_width_ft_ST" makes bike_lane_SUM_ST1 more sensical
            "bikeway_width_ft_ST")) %>%
  # make orderd vars numeric to reduce variables in model (can revert or re-bin if very non-linear)
  mutate_if(is.ordered, as.numeric) %>%
  mutate(comfort_rating_ordered = d$comfort_rating) #Outcome as a factor

names(d.model)


# -----------------------------------------------------------------------------------
# 3. Regression models ----

#     Models with only street features and video names ----
# Do street features explain as much as street id?
# Basically YES, but we also see from this that there is lots of variation in ratings within videos
# (YES is potentially because they essentially reconstruct names, but coef seem reasonable, so prob. not)

# Model from names (explains very slightly more variation)
lm.video_name = lm(as.numeric(comfort_rating) ~ video_name, data = d)
summary(lm.video_name)

# Model from features (8 fewer variables than above, only 2% loss in R-squared, and coef make sense)
d.model.street = d %>% select(matches("(ST|comfort_rating)", ignore.case = FALSE)) %>%
  #remove some vars strongly correlated with others
  select(-c("veh_volume_ST", "comfort_rating_3lev","NCHRP_BLOS_score_ST", #lower score -> A
            "prevailing_speed_mph_ST",
            "bike_lane_and_parking_lane_width_ft_ST",
            #"shoulder_width_ft_ST",
            #"speed_limit_mph_ST",
            "HCM_BLOS_ST", "LTS_ST", 
            "bikeway_width_ft_ST",
            "urban_ST", 
            "protected_ST", "buffer_ST", "bike_lane_blocked_ST", "bike_lane_SUM_ST")) 

lm.street = lm(as.numeric(comfort_rating) ~ ., data = d.model.street)
summary(lm.street) 

plot(predict(lm.street), predict(lm.video_name)); abline(a=0,b=1)

#     Preliminary overall models ----

# Numeric response (don't use)
lm.prelim = lm(comfort_rating ~ . , data = d.model %>% select(-"comfort_rating_ordered"))
summary(lm.prelim)

# Ordered response (OK)
d.model.ord =  d.model %>% select(-"comfort_rating")
ord.prelim = polr(comfort_rating_ordered ~ ., data = d.model.ord, Hess = TRUE)
summary(ord.prelim)

#     Penalized models (GLMNET, lasso) ----

glmcr.prelim = glmnetcr(x = model.matrix(ord.prelim)[,-1], y = model.frame(ord.prelim)$comfort_rating_ordered)
names(glmcr.prelim)
png("IMG/penalized_ordered_coef_path.png", width = 1600, height = 1000)
par(mai  = c(2,2,2,2))
plot.glmnetcr(glmcr.prelim) # I manually added a cex.axis = .5 arg
dev.off()
nonzero.glmnetcr(glmcr.prelim, s = 20)
sort(nonzero.glmnetcr(glmcr.prelim, s = 21)$beta[1:16])

#Note how buffered bike lane and bike lane and parking lenght width trade off

#     Look at outliers ####

# We find some data that we may consider outliers, but enough that we should perhaps involve it in modelling, e.g. person random effect?

head(sort(lm.video_name$residuals), 30)
tail(sort(lm.video_name$residuals), 30) 
head(sort(lm.street$residuals), 30)
tail(sort(lm.street$residuals), 30) 

which.max(abs(lm.video_name$residuals)) #14705
filter(d, person_ID == d$person_ID[14705]) #Ranked all video "very uncomforable", even though black was between and NCHRP scores were two E's and two A's
which.max(abs(lm.street$residuals)) #10087
d$person_ID[c(which.max(abs(lm.video_name$residuals)), which.max(abs(lm.street$residuals))) ] #140, 140


# Look at entries where respondant had no variance but group was "Between" 
#View(d %>% mutate(x = as.numeric(comfort_rating)) %>% group_by(person_ID) %>% 
#  summarize(y = max(x) - min(x), type = first(VideoGroup)) %>% 
#  arrange(y, type))
#
#View(d %>% mutate(x = as.numeric(comfort_rating)) %>% group_by(person_ID) %>%
#            mutate(y = max(x) - min(x)) %>% filter(y == 0) %>% arrange(person_ID))

# ------------------------------ 4. Scrtach -----------------------------------------
#     Ordered logit with person random effect? ####

library("ordinal")

d.model.randord =  d.model %>% select(-"comfort_rating") %>% mutate(person_ID = as.factor(d$person_ID))
names(d.model.randord)

# randord.prelim = clmm2(comfort_rating_ordered ~ ., random=person_ID, data = d.model.randord)

# doesn't work:
# (and attempt with binary response got same error)
# randord.prelim = clmm2(comfort_rating_ordered ~ ., random=person_ID, data = d.model.randord)
# Error in setStart(rho) : attempt to find suitable starting values failed
#In addition: Warning messages:
#  1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 


#     Decision tree ----

library(rpart)
library(rpart.plot)
#Set response type
mf3 = model.frame( comfort_rating_3lev ~ ., 
                  data = d.model %>% select(-c("comfort_rating", "comfort_rating_ordered")) %>% 
                   mutate(comfort_rating_3lev = d$comfort_rating_3lev) )

tree.prelim = rpart(mf3, control = rpart.control(minsplit = 5, minbucket = 2, cp = .001),
                    model = TRUE, method = "class")

rpart.plot(tree.prelim, digits = 1, cex = .6, type = 3)

#     binary outcome ----

# Use comfort_rating_3lev and remove neutrals
lm.binary.1 = glm(as.numeric(comfort_rating_3lev) - 1 ~ .,
                  data = d.model %>% mutate(comfort_rating_3lev = d$comfort_rating_3lev) %>% 
                    select(-c("comfort_rating", "comfort_rating_ordered")) %>%
                    filter(comfort_rating_3lev !="neutral") %>% droplevels, family = "binomial")

summary(lm.binary.1)

