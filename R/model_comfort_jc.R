# Models of bike comfort data
# Jane Carlen, May 2019
# -----------------------------------------------------------------------------------
#
# Questions: ----
# - Levels: L, Q, C, 4 ?? <-- uue to ordered input. check back on ordered contrasts
# (- How could table(d$bike_speed_mph_ST) get to 51 and 61 mph? e.g. https://youtu.be/OpZ-zH7HD6o <- removed this var)
#
# To do: ----
# - We didn’t get as change to look at which videos had high random effects but I’ll do that when everything’s done, in part to to see if there are any interaction effects that seem obviously lacking. 
# - random effects in ordered logit
#     - do we want a stronger prior? Is additive correct?
#     - consider category-speciric effects or priors for any terms? (see brms JSS paper)
# - linear vs non.linear effects of variables? What I'm seeing with interactions may indicate transformations needed.
#
# - try a "kid 17 or under" treating NA as no kids and see if any effect
# - FINAL BIKEWAY WIDTH: parking + bike lane + buffer + operating space (bike lane width + shoulder space not taken by parking)
# speed limit groups = 25,  30 - 35, 45-50 (chosed based on speed group and where their prevailing speeds seem to be)
# don't include prevail either on it's own or as a difference with speed
  # prevailing speed (which would be harder for planners, and it's unclear how it's calculated, plus the car it was calculated on might be cut out of our snipped). MAYBE include prevail - speed.
    # see if outside lane width comes back -- try it as a class variable?, less or more than 10ft ->
    # might be individative how much vehicle traffic and how fast they're going

# Notes: ----
#     1. So far nothing major has come from looking at interaction effects. What I'm seeing may indicate that the effects of opinion variables are non-linear (if still treating that as numeric, not as factors)
#     2. Removed housing cost as a variable because it has a fairly large amount of missing data, 
#       has some significant outliers and potentially bad data (undergrads paying 5000/month in davis>)
#       and, most importantly, the housing costs in a college town/a majority student population aren't generalizable.
#       See scratch notes below.
#       Left it in preliminary models to show the direction is generally negative, as expected.
#     3. Ended up with a very simple model to impute age when missing (just based on primary role)
#       because other variables offered only minor improvements and had some missing data
#     4. Considered imputing gender, but would those people with NA have not answered intentionally?
#       For exploring imputations and brms used character "NA" factor level for those entries, akin to "other"
#       Excluding other NAs in bayesian model for now so I can treat opinion variables as numeric (for convencience)
#         Not sure how to integrate "NA" level into ordered scale otherwise
#     5. Dillon: added two new variables:
#       (1) "bike_boulevard" just to keep track of it (we won't use it)
#       (2)  "bike_operating_space" the key measurement variable to use
#       The trick with bike_operating_space is that the effect should be non-linear. For example, on a slow speed mixed traffic (bike and cars in same lane), operating space will be 0, but the road is likely to be relatively comfortable compared to a major arterial. However, when traffic speeds and volumes are high, more operating space should equal more comfort. Does that make sense? So i guess what I'm saying is I expect an interaction between operating space and our new speed limit variable. 

# -----------------------------------------------------------------------------------------------------------------------------------------------------------
# 0. Setup ####
#   Packages ----
#library(MASS)
library(dplyr) # after MASS to use dplyr select
library(glmnetcr)
library(brms)
library(forcats)
library(ggridges)
library(shinystan)
library(cowplot)
library(rpart)
library(rpart.plot)

#   Other ----
setwd("~/Documents/videosurvey")
source("R/data_comfort.R")
source("R/eda_comfort.R")

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
View(.Last.value)

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
  select(-c("edu_self", "child6", "child615", "child1617", "monthly_housing_cost_class", "hh18_older",
            "usual_mode", "secondary_mode"))

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
View(.Last.value)
# -----------------------------------------------------------------------------------
# 2. Transforming variables -----

d$bike_access = as.factor(d$bike_access)
names(d)[sapply(d, is.factor)]


# May want to reduce child fields to binary, but note alot of missing data in these fields
levels(d$child1617) = c(NA,0,1,2,0)
levels(d$child6) = c(NA,0,1,2,3,0)
levels(d$child615) = c(NA,0,1,2,3,4,0)
table(as.numeric(as.character(d$child1617)), useNA = "always")

d$child6 = as.numeric(as.character(d$child6))
d$child615 = as.numeric(as.character(d$child615))
d$child1617 = as.numeric(as.character(d$child1617))
d$child_u18 = rowSums(d[,c('child6', 'child615', 'child1617')], na.rm = T) > 0 # treats NAs as zeros  

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
                          "VideoGroup", "video_name")) %>%
  # hh18_older has weird entries and overlaps with household type anyway.
  # hh_composition is also weirdly coded. I recoded to 4-level.
  # housing_unit_type also has lots of missing data   
  # rent_split is similar to rent_share but more missing data
  # keep rent share (more descriptive with less missing data)
  # edu_self has too-high correlation with primary role. edu_parent has lots of missing data
  select(-c("hh18_older", "hh_composition", "housing_unit_type",
            "rent_split", 
            "edu_self", "edu_parent")) %>%
  # Only two blocked ones and I looked at the videos -- neither actually seems "blocked"
  select(-c("bike_lane_blocked_ST")) %>%
  # removed bike_speed_mph_ST bc of data quality question (see top)
  select(-c("bike_speed_mph_ST")) %>%
  # bike_operating_space_ST is just a tracking variable
  # select(-c("bike_operating_space_ST")) %>%
  # Removed in favor of other version or similar var
  select(-c("op_like_biking_3lev", "comfort_rating_3lev",
            "monthly_housing_cost", "veh_volume_ST", #<- used veh_volume2_ST instead, more precise
            "NCHRP_BLOS_ST", "HCM_BLOS_ST",
            "usual_mode", "prevailing_speed_mph_ST", #<- used prevail - speed instead to reduce correlation          #correlated with bike_lane_SUM_ST:
            "bike_lane_and_parking_lane_width_ft_ST",  "bikeway_width_ft_ST", "shoulder_width_ft_ST",
            "speed_limit_mph_ST", #use 3lev instead
            #in bike_lane_SUM_ST
            "bike_lane_ST", "protected_ST", "buffer_ST",
            # tracking variable, not complete
            "bike_boulevard_ST",
            # tried just secondary mode is BIKE instead
            "secondary_mode",
            # removed because too correlated to age and primary role, and specific to college town
            "monthly_housing_cost_class")) %>%
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
            "op_eco_concern", "bike_access", 
            "op_travel_wasted", "op_like_driving",
            "op_schedule_transit", "op_limit_driving", "op_need_own_car",
            "op_LIKE_COMMUTE_SUM", "secondary_mode_BIKE",
            "op_bike_often", # mostly captured by bike commuting
            "hh_composition_4lev",
            "urban_ST",
            "rent_share")) %>%
  # make orderd vars numeric to reduce variables in model (can revert or re-bin if very non-linear)
  mutate_if(is.ordered, as.numeric) %>%
  mutate(comfort_rating_ordered = d$comfort_rating)

names(d.model)

# ----------------------------------------------------------------------------------
# 3  Missing Data (age) ----
#   Data set (by person) of person-level variables for potential imputation & filtering ----

d.person = d %>% 
  dplyr::select(-contains("ST", ignore.case = FALSE)) %>%
  dplyr::select(-contains("video", ignore.case = TRUE)) %>%
  dplyr::select(-contains("comfort_rating", ignore.case = TRUE)) %>%
  dplyr::select(-contains("3lev")) %>%
  dplyr::select(-c("usual_mode", "hh_composition")) %>% # use 4lev instead
  dplyr::select(-c("URL")) %>%
  group_by(person_ID) %>%
  summarise_all(list(first)) %>%
  # keep only person_ID for later identification
  dplyr::select(-c("ID", "block_ID", "sub_ID", "ID", "cts_ID")) %>%
  # too much NA
  dplyr::select(-contains("child")) %>%
  dplyr::select(-c( "rent_split", "housing_unit_type", "edu_parent",
             "hh18_older", "edu_self")) %>%
  mutate(female = replace(female, is.na(female), "NA"))

#       how much missing data? ---- 

# percentages of missingness
sort(apply(d, 2, function(x) {mean(is.na(x))}))

# generally rows with a lot of missing data have many missing opinion variables
# @View(filter(d.person, rowSums(is.na(d.person)) >= 10))$person_ID
sort(colSums(is.na(d.model)))
table(rowSums(is.na(d.person))) #lose 384 rows, or 79 individuals (out of 15288 and 3089 respectively)

#       Explore age variable (ended up with very simple model) ----

# Similar pattern for age and primary role as housing cost, not surprising
ggplot(d, aes(x = age)) + 
  geom_histogram() + 
  facet_wrap(~primary_role, scales = "free_y", ncol = 1)

d.person.age = d.person %>% 
  #just in case
  dplyr::select(-contains("age_impute")) %>%
  #repeat other vars
  dplyr::select(-c("secondary_mode", "monthly_housing_cost_class")) %>%
  # too much NA and not significant anyway
  dplyr::select(-c("distance")) %>%
  # Use commute sum variable instead, indiv. vars generally not signif anyway
  dplyr::select(-c("op_schedule_transit", "op_commute_goes_well",
            "op_commute_best_imagine", "op_satisfied_with_commute",
            "op_commute_positive_feelings", "op_commute_no_change",
            # took out more vars in favor of simpler imputation model, fewer excluded entries
            "op_commute_positive_negative", "op_like_biking", "op_feel_safe",
            "op_like_transit", "op_like_driving", "op_like_driving", "op_travel_wasted",
            "op_arrive_professional", "op_bike_often", "op_arrive_professional",
            "op_need_car", "op_travel_stress", "op_limit_driving", "op_need_own_car",
            "op_smartphone",
            "bike_access", "secondary_mode_BIKE", "bike_ability", "usual_mode_4lev",
            #too much missing in rows also missing age, impairs imputation:
            "monthly_housing_cost", "rent_share", "hh_composition_4lev", 
            "license", "op_LIKE_COMMUTE_SUM", "comfort_four_no_lane", "female",
            "op_eco_concern"
  )
  )

names(d.person.age)
colSums(is.na(d.person.age))

# For lm, residual plot showwed some fanning and effects of restricted age (can't be below 18) , 
# -> poisson glm respects the minimum and residuals look more balanced 
#    (we expect variance to go up with age, to some extent)
# Max outliers are outliers are for two 54 yr. old undergrads

lm.age = lm(age - 18 ~.,  data = d.person.age %>% select(-"person_ID")) 

glm.age = glm(age - 18 ~.,  data = d.person.age %>% select(-"person_ID"), #%>%
              #filter(! (grepl(primary_role, pattern = "Undergraduate") & age > 50) ),
              family = "poisson")

par(mfrow = c(1,2))
hist(resid(glm.age, type = "response")); hist(resid(lm.age))
quantile(resid(glm.age, type = "response"), c(.005,.05,.9,.995))
quantile(resid(lm.age, type = "response"), c(.005,.05,.9,.995))
summary(glm.age); summary(lm.age)
dim(model.matrix(glm.age)) #lose ~ 7pct of data, including 45 na's in age collected
plot(glm.age)
plot(lm.age)
#age means by role
#d.person %>% group_by(primary_role) %>% summarize(mean(age, na.rm = T))
#compare glm and lm predictions
#plot(sort(18 + predict(glm.age, type = "response")), type = 'l', ylim = c(15,75)); 
#points(sort(18 + predict(lm.age)), col ="red", type = "l")

#How many rows do we lose?
rowSums(is.na(d.person.age %>% filter(is.na(age)) %>% select(-"age")))
colSums(is.na(d.person.age %>% filter(is.na(age)) %>% select(-"age")))

#       age imputation ----
age.impute = 18 + predict(glm.age, newdata = d.person %>% filter(is.na(age)), type = "response")       
d.person$age_impute = d.person$age
d.person$age_impute[is.na(d.person$age)] = age.impute

names(age.impute) = (d.person %>% filter(is.na(age)))$person_ID
d.model$person_ID = d$person_ID #works if same dim
d.model$age_impute = d.model$age
d.model.age.impute =  age.impute[as.character(d.model$person_ID)]
d.model$age_impute[!is.na(d.model.age.impute)] = d.model.age.impute[!is.na(d.model.age.impute)]
d.model = d.model %>% dplyr::select(-c("age"))

#       Compare to cross-validated model (if more complicated model for age): 

# #Hold out set with vanilla poisson glm
# s = sample(1:nrow(d.person.age))[1:300] #hold-out set; make large enough to capture factor levels
# glm.age = glm(age - 18 ~.,  data = d.person.age[-s,] %>% select(-"person_ID"), family = "poisson")
# test.resids = d.person.age[s,"age"] - (18 + predict(glm.age, newdata = d.person.age[s,], type = "response"))    
# hist(test.resids[[1]], breaks = 20)
# quantile(test.resids[[1]], c(.005,.995), na.rm = T)
# 
# cvglm.age = cv.glmnet(model.matrix(glm.age), glm.age$y)
# #plot(cvglm.age)
# coef(cvglm.age, s = cvglm.age$lambda.min)
# s.x = model.matrix(age ~ ., data = d.person.age[s,] %>% select(-c("person_ID")))
# 
# cv.test.resids = d.person.age[s[-which(rowSums(is.na(d.person.age[s,])) > 0)],"age"]  - 
#   (18 + predict.cv.glmnet(cvglm.age, newx = s.x, type = "response", s = cvglm.age$lambda.min))
# hist(cv.test.resids[[1]], breaks = 20)
# quantile(cv.test.resids[[1]], c(.05,.95))

# -----------------------------------------------------------------------------------
# 4. Preliminary fixed-effect models ----


#     i.   Models with specific subsets of effects ----
#     - Scores and Bike lane ----

# None of these models perform particularly well. The residual deviance not much lower than for a null model. 
# The key observation is that the model with bike lane (levels are none, lane, protected lane) does better than any of the scores on their own.
#     Even the model with all three scores (8 predictors), does only slightly better than the bike lane model with only two predictors, and if we add speed limit to the bike lane model (still only 3 predictors) it outperforms the mdoel with all three scores

#Null model:
summary(MASS::polr(as.ordered(comfort_rating) ~ 1, data = d, Hess = TRUE)) #Residual Deviance: 58676.76 

#Score models:
summary(MASS::polr(as.ordered(comfort_rating) ~ NCHRP_BLOS_score_ST, data = d, Hess = TRUE)) # Residual Deviance: 58367.77 
summary(MASS::polr(as.ordered(comfort_rating) ~ as.factor(HCM_BLOS_ST), data = d, Hess = TRUE)) #Residual Deviance: 57619.62
summary(MASS::polr(as.ordered(comfort_rating) ~ as.factor(LTS_ST), data = d, Hess = TRUE)) #Residual Deviance: 57811.67 
summary(MASS::polr(as.ordered(comfort_rating) ~  as.factor(LTS_ST) + as.factor(HCM_BLOS_ST) + 
                     NCHRP_BLOS_score_ST, data = d, Hess = TRUE)) #Residual Deviance: 56703.81 
#By Video
summary(MASS::polr(as.ordered(comfort_rating) ~ as.factor(video_name), data = d, Hess = TRUE)) #55252.60

# Bike lane models:
summary(MASS::polr(as.ordered(comfort_rating) ~ as.factor(d.model$bike_lane_SUM_ST), 
                   data = d, Hess = TRUE)) #Residual Deviance: 56876.68 
summary(MASS::polr(as.ordered(comfort_rating) ~ d.model$bike_operating_space_ST, 
                   data = d, Hess = TRUE)) #Residual Deviance: 57841.45 
summary(MASS::polr(as.ordered(comfort_rating) ~ d.model$speed_limit_mph_ST_3lev + 
                     as.factor(d.model$bike_lane_SUM_ST), data = d, Hess = TRUE)) #Residual Deviance: 56260.20 


# see "Models with only street features" for expansion of this 
                                      
#     - ST and video name ----
# Do street features explain as much as street id?
# Basically YES, but we also see from this that there is lots of variation in ratings within videos
# (YES is potentially because they essentially reconstruct names, but coef seem reasonable, so prob. not)

# Model from names (explains very slightly more variation)
lm.video_name = lm(as.numeric(comfort_rating) ~ video_name, data = d)
summary(lm.video_name)

# Model from features (far fewer variables than above, only 2% loss in R-squared, and coef make sense)
d.model.street = d %>% select(matches("ST|comfort_rating", ignore.case = FALSE)) %>%
  #remove some vars strongly correlated with others
  select(-c("veh_volume_ST", "comfort_rating_3lev","NCHRP_BLOS_score_ST", #lower score -> A
            "prevailing_speed_mph_ST",
            #"speed_prevail_minus_limit_ST",
            "speed_limit_mph_ST", # use prevailing speed here bc more efficient than these two speed vars, but we'll use speed categories in final models bc can be more consistently applied out of our sampel, better for planning purposes.
            "bike_speed_mph_ST",
            "bike_lane_and_parking_lane_width_ft_ST",
            #"shoulder_width_ft_ST",
            "bike_lane_ST",
            "outside_lane_width_ft_ST",
            #bike_lane_SUM_ST"
            "bike_boulevard_ST", #tracking variable, not for use
            "HCM_BLOS_ST", "LTS_ST", 
            "NCHRP_BLOS_ST",
            "bikeway_width_ft_ST",
            "urban_ST", 
            "num_lanes_ST",
            "protected_ST", 
            "buffer_ST", 
            "bike_lane_blocked_ST")) 

lm.street = lm(as.numeric(comfort_rating) ~ . + 
                 bike_operating_space_ST*speed_limit_mph_ST_3lev,
               data = d.model.street)
summary(lm.street) 
apply(cor(model.matrix(lm.street)), 2, function(x) {max(abs(x[x!=1]), na.rm = T)})
plot(predict(lm.street), predict(lm.video_name)); abline(a=0,b=1)

#     + Look at outliers ####

# We find some data that we may consider outliers, but enough that we should perhaps involve it in modelling, e.g. person random effect?

head(sort(lm.video_name$residuals), 5)
tail(sort(lm.video_name$residuals), 5) 
head(sort(lm.street$residuals), 30)
tail(sort(lm.street$residuals), 30) 

which.max(lm.video_name$residuals) #14705
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

#     ii.  Models allowing all main effects ----

#     - Numeric response (don't use) ----
lm.prelim = lm(comfort_rating ~ . , data = d.model %>% 
                 select(-c("comfort_rating_ordered", "person_ID")))
summary(lm.prelim)

#     - Ordered response (OK) ----

ord.prelim = MASS::polr(comfort_rating_ordered ~ . - comfort_rating - person_ID - video_name, 
                        data = d.model %>%  
                          # comfort 4lane and video volume seems to have non-linear effects
                          mutate(comfort_four_no_lane = as.factor(comfort_four_no_lane),
                                 veh_volume2_ST = as.factor(veh_volume2_ST),
                                 video_name = d$video_name), #<- for later 
                        Hess = TRUE)
summary(ord.prelim)

#     - Penalized ordered response (GLMNET, lasso) ----

glmcr.prelim = glmnetcr(x = model.matrix(ord.prelim)[,-1], y = model.frame(ord.prelim)$comfort_rating_ordered)
par(mfrow = c(1,1))
plot(glmcr.prelim$dev.ratio)
par(mai  = c(1,1,1,3))
plot.glmnetcr(glmcr.prelim) 
s = 26 # isolate top effects (incl two levels of bike land) 
# This model is more parsimonious (23 var), the effects make sense, and it's backed up by the elbow of the deviance plot
round(nonzero.glmnetcr(glmcr.prelim, s = s)$beta, 3)
top.names = names(nonzero.glmnetcr(glmcr.prelim, s = s)$beta)
top.names = top.names[!grepl(top.names, pattern = "cp[0-9]{1}")]           
top.names

# Second round after trimming -- easier to read plot
glmcr.prelim2 = glmnetcr(x = model.matrix(ord.prelim)[,colnames(model.matrix(ord.prelim)) %in% top.names], 
                         y = model.frame(ord.prelim)$comfort_rating_ordered)
png("IMG/penalized_ordered_coef_path.png", width = 1600, height = 1000 )
par(mai  = c(1,1,1,3))
plot.glmnetcr(glmcr.prelim2) # I manually added a cex.axis = .5 arg
dev.off()


#     Models allowing main AND interaction effects ----
#     - Hard-coding some potential interactions ----

round(prop.table(table(d$comfort_rating_3lev, d$bike_operating_space_ST, d$veh_volume2_ST), c(2,3)), 2)

d.model.int = data.frame(glmcr.prelim2$x) %>% mutate(
  comfort_rating_ordered = glmcr.prelim2$y,
  veh_vol_non0_opspace_0_ST = as.numeric((veh_volume2_ST2 > 0|veh_volume2_ST3 > 0) &  bike_operating_space_ST < 3)
  # add one for speed and operating space?
  # street_park_opsace_low = street_parking_ST == 1 & bike_operating_space_ST < 8
  )

#     - Looking for potential interactions ----
#       - Decision tree exploration ----

# As with models, most of what comes out is relationships between street variables and opinions related to biking

#Set response type
mf3 = model.frame( as.numeric(comfort_rating_ordered) > 3 ~ ., 
                   data = data.frame(d.model.int))

tree.prelim = rpart(mf3, control = rpart.control(minsplit = 5, minbucket = 2, cp = .005),
                    model = TRUE, method = "class")

par(mfrow = c(1,1))
rpart.plot(tree.prelim, digits = 1, cex = .6, type = 3)

# Tried looking at all potential interactions. too difficult to sort through this. 
# It again seems like more evidence of factors in need of transformation

#ord.prelim.int.mm = model.matrix(comfort_rating_ordered ~ .*., data = data.frame(comfort_rating_ordered = glmcr.prelim2$y, glmcr.prelim2$x))
#glmcr.prelim.int = glmnetcr(x = ord.prelim.int.mm[,-1], y =glmcr.prelim2$y)
#plot(glmcr.prelim.int$dev.ratio)
#sort(nonzero.glmnetcr(glmcr.prelim.int, s = 10)$beta)
#names(nonzero.glmnetcr(glmcr.prelim.int, s = 10)$beta)
#plot.glmnetcr(glmcr.prelim.int) #


#     - Ordered response with interactions ----

# just considering interactions of street varaibles for now, specifically between features of traffic and available biking space 
#    -- haven't added random effects for individuals yet
#    -- found that with unpenalized model the interactions often made main effects switch directions in non-intuitive ways,
#       and their signs were also often counterintuitive

ord.prelim.int = MASS::polr(comfort_rating_ordered ~ . +   
                           # consider interactions 
                            street_parking_ST:bike_lane_SUM_ST1 +
                            street_parking_ST:bike_lane_SUM_ST2 +
                            street_parking_ST:bike_operating_space_ST + 
                            veh_volume2_ST3:bike_lane_SUM_ST1 +
                            veh_volume2_ST3:bike_lane_SUM_ST2 +
                            # have veh_vol_non0_opspace_0_ST instead:
                             #veh_volume2_ST3:bike_operating_space_ST +
                            #speed_limit_mph_ST_3lev.40.50.:bike_lane_SUM_ST1 + <- only one street in the 45-50 range doesn't have a bike lane, and none are protected
                            #speed_limit_mph_ST_3lev.40.50.:bike_lane_SUM_ST2 +
                            #speed_limit_mph_ST_3lev.40.50.:outside_lane_width_ft_ST + # <- not significant here or in RandEff models
                            speed_limit_mph_ST_3lev.40.50.:bike_operating_space_ST +
                            speed_limit_mph_ST_3lev.40.50.:veh_volume2_ST3 +
                            speed_limit_mph_ST_3lev.30.40.:veh_volume2_ST3
                           , 
                            #speed_prevail_minus_limit_ST:bike_lane_SUM_ST1 +
                            #speed_prevail_minus_limit_ST:bike_lane_SUM_ST2 +
                            #speed_prevail_minus_limit_ST:bike_operating_space_ST,
                   data = d.model.int, Hess = TRUE)
summary(ord.prelim.int)

#     - Penalized ordered response with interactions (GLMNET, lasso) ----

# Still see some weird flips of main effects
glmcr.int = glmnetcr(x = model.matrix(ord.prelim.int)[,-1], y = ord.prelim.int$model$comfort_rating_ordered)
s = 30
round(nonzero.glmnetcr(glmcr.int, s = s)$beta, 3)
plot(glmcr.int$dev.ratio)
#plot.glmnetcr(glmcr.int)  #slow
top.names.int = names(nonzero.glmnetcr(glmcr.int, s = s)$beta)
top.names.int = top.names.int[!grepl(top.names.int, pattern = "cp[0-9]{1}")]           
top.names.int

# ----------------------------------------------------------------------------------
# 5. Mixed models with person random effects (BRMS) ---------------------------
#       + Exploratory: Figure out how to account for between vs. within in the model ----
  
  #--- Note: blocks don't reprsent between or within selections (as I has thought), rather they represent types of videos ("loosely based on bike infrastructure, whether the road was a collector or arterial, and speed/traffic of cars")
  # so a video is always in the same block, but the person may see it as within (i.e. they only see it with other videos in the same block [in which case they have uniform block_ID]), or between (e.g  the only see it with other videos NOT within the same block[in which case they have no duplicated block_ID]). The block corresponds to broad road type, but the video_group is just an experimental variable. There should roughly be balance across blob and video_group. clear as mud?
  
  # In short *person_ID*, not block_ID, is either between or within 
  #---
  
  # Compare those who received "between" vs. "within" ---
  
  #mean - between stays closer to the center
  d %>% group_by(person_ID) %>% 
  summarize(mean = mean(as.numeric(comfort_rating)), type = first(VideoGroup)) %>%
  ggplot() + geom_density(aes(x = mean, fill = type), alpha = .5)

#variance - within variance more likely to be small
d %>% group_by(person_ID) %>% 
  summarize(var = var(as.numeric(comfort_rating)), type = first(VideoGroup)) %>%
  ggplot() + geom_density(aes(x = var, fill = type), alpha = .5)

# Next tables summarizes that those who saw between tended to have higher variance in their ratings, but less variance in their means (summarizing the plots above). Also there's no major difference in mean of means which is good, i.e. the two groups have similar coverage and shouldn't have different intercepts. 

d %>% group_by(person_ID) %>% 
  summarize(var = var(as.numeric(comfort_rating)),
            mean = mean(as.numeric(comfort_rating)),
            type = first(VideoGroup)) %>% 
  group_by(type) %>% summarize(mean_of_means = mean(mean),
                               mean_of_variances = mean(var, na.rm = T),
                               variance_of_means = var(mean))

# Something of a negative relationship between median score and variance, but this
# may be addressed by the logit transformation 

d %>% group_by(video_name) %>%
  summarize(var = var(as.numeric(comfort_rating), na.rm = T),
            med = median(as.numeric(comfort_rating)),
            n = n()) %>% arrange(var)

summary(lm(d.video$var ~ d.video$median_comfort))
#       - build and scale (all on 0-1) model frames: ----
#          + Main effects set: Built after looking at random effects models and merging "inclusive" & "exclusive" sets: ----

d.remodel.me = d.model %>%
                select(-"comfort_rating") %>%
                select(-c("num_lanes_ST", "primary_role", "divided_road_ST",
                       "pavement_condition_ST")) %>% #<- could try adding some of these back in
                mutate(person_ID = as.factor(person_ID),
                       video_name = as.factor(d$video_name),
                       #create this variable before converting components
                       veh_vol_non0_opspace_0_ST = as.numeric(veh_volume2_ST > 1 &  
                                                                bike_operating_space_ST < 3),
                       veh_volume2_ST = as.factor(veh_volume2_ST),
                       comfort_four_no_lane = as.factor(comfort_four_no_lane),
                       street_parking_ST = as.factor(street_parking_ST),
                       
                       #reorder to keep bike level later
                       usual_mode_4lev = factor(usual_mode_4lev, rev(levels(usual_mode_4lev) )) ) %>%
                #rest of numeric vars get put on 0-1 scale
                mutate_at(vars(contains("op")), function(x) {x/5}) %>%
                mutate_at(names(.)[which(sapply(., function(x) {is.numeric(x) && max(x, na.rm = T) > 1}))],
                          function(x) {(x - min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)}) %>%
                #so we don't have to lose this data and respect "NA" responses:
                mutate(female = as.factor(replace(female, is.na(female), "NA")))

d.remodel.me = data.frame(model.matrix(comfort_rating_ordered ~ . - person_ID - video_name, data = d.remodel.me),
                          comfort_rating_ordered = 
                            model.frame(comfort_rating_ordered ~ . - person_ID - video_name, data = d.remodel.me)$comfort_rating_ordered,
                          person_ID = 
                            model.frame(person_ID ~ . - comfort_rating_ordered - video_name, data = d.remodel.me)$person_ID,
                          video_name = 
                            model.frame(video_name ~ . - comfort_rating_ordered - person_ID, data = d.remodel.me)$video_name
                          )

d.remodel.me = d.remodel.me %>% select(-c("X.Intercept.", "femaleNA","usual_mode_4levCar", "usual_mode_4levPublic.Trans"))
str(d.remodel.me)

#         - scaling (put all on 0-1 scale by subtracting min and dividing by max)

d.remodel.me = d.remodel.me %>% mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})
str(d.remodel.me)

#          + Main effects set + interactions: ----

ord.int = MASS::polr(comfort_rating_ordered ~ . -video_name - person_ID +   
                              # consider interactions 
                              street_parking_ST1:bike_lane_SUM_ST1 +
                              street_parking_ST1:bike_lane_SUM_ST2 +
                              street_parking_ST1:bike_operating_space_ST + 
                              # we decided to only interact on highest level of volume
                              veh_volume2_ST3:bike_lane_SUM_ST1 +
                              veh_volume2_ST3:bike_lane_SUM_ST2 +
                              # have veh_vol_non0_opspace_0_ST instead:
                                # veh_volume2_ST3:bike_operating_space_ST +
                              # speed_limit_mph_ST_3lev.40.50.:bike_lane_SUM_ST1 + <- only one street in the 45-50 range doesn't have a bike lane, and none are protected
                              # speed_limit_mph_ST_3lev.40.50.:bike_lane_SUM_ST2 +
                              # speed_limit_mph_ST_3lev.40.50.:outside_lane_width_ft_ST + <- mean ~0 with wide var. in rand.eff model
                              speed_limit_mph_ST_3lev.40.50.:bike_operating_space_ST +
                              speed_limit_mph_ST_3lev.40.50.:veh_volume2_ST3 + # <- new
                              speed_limit_mph_ST_3lev.30.40.:veh_volume2_ST3 # <- new
                       ,
                            #speed_prevail_minus_limit_ST:bike_lane_SUM_ST1 +
                            #speed_prevail_minus_limit_ST:bike_lane_SUM_ST2 +
                            #speed_prevail_minus_limit_ST:bike_operating_space_ST,
                            data = d.remodel.me, Hess = TRUE)
summary(ord.int)

d.remodel.int = data.frame(model.matrix(ord.int)[,-1],
                           person_ID = as.factor(ord.int$model$person_ID),
                           video_name = as.factor(ord.int$model$video_name),
                           comfort_rating_ordered = ord.int$model$comfort_rating_ordered)
str(d.remodel.int)

#         - scaling (put all on 0-1 scale by subtracting min and dividing by max) 

d.remodel.int = d.remodel.int %>% mutate_if(is.numeric, 
                function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})

 
#          + (DEPRECATED) ----
#                 + (X) Inclusive coefficient set: ----All levels of selected coefficients ----

# d.remodel = d.model %>% 
#                 select(-"comfort_rating") %>%
#                 select(-c("num_lanes_ST", "primary_role", "child_u18", "divided_road_ST", 
#                        "pavement_condition_ST")) %>% #<- could try adding some of these back in 
#                 mutate(person_ID = as.factor(person_ID),
#                        video_name = as.factor(d$video_name),
#                        veh_volume2_ST = as.ordered(veh_volume2_ST),
#                        comfort_four_no_lane = as.ordered(comfort_four_no_lane),
#                        street_parking_ST = as.factor(street_parking_ST)) %>%
#                 #rest of numeric vars get put on 0-1 scale
#                 mutate_at(vars(contains("op")), function(x) {x/5}) %>%
#                 mutate_at(names(.)[which(sapply(., function(x) {is.numeric(x) && max(x, na.rm = T) > 1}))], 
#                           function(x) {(x - min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)}) %>%
#                 #so we don't have to lose this data and respect "NA" responses:
#                 mutate(female = as.factor(replace(female, is.na(female), "NA")))
# str(d.remodel)
# 
# #         - scaling (put all on 0-1 scale by subtracting min and dividing by max) 
# 
# d.remodel = d.remodel %>% mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})


# Full scaling (decided on scaling as above instead)
# d.remodel.mat = cbind(
#    apply(d.remodel.mat[,-which(colnames(d.remodel.mat)=="person_ID")], 2, scale, center = TRUE, scale = T), 
#    person_ID = d.remodel.mat[,which(colnames(d.remodel.mat)=="person_ID")]) #had to exclude person_ID from scaling
#  
#   d.remodel.df = data.frame(d.remodel.mat)
#   d.remodel.df$comfort_rating_ordered = d.model$comfort_rating_ordered[rowSums(is.na(d.remodel))==0]
#   d.remodel.df$video_name = d$video_name[rowSums(is.na(d.remodel))==0]

#                 + (X) Exclusive coefficient set -- only top fx from pen. ord. model and hardcoded ineractions ----

# may only include some levels of certain factors

# str(d.model.int)
# dim(d.model.int)
# 
# d.remodel.int = d.model.int %>% 
#   mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)}) %>%
#   mutate(person_ID = as.factor(ord.prelim$model$person_ID),
#          video_name = as.factor(ord.prelim$model$video_name))
# 
# #         - scaling (put all on 0-1 scale by subtracting min and dividing by max) 
# 
# d.remodel.int = d.remodel.int %>% mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})


#                 + (X) Exclusive coefficient set+ interactions: ----
# 
# d.remodel.int2 = data.frame(glmcr.int$x[,top.names.int %in% colnames(glmcr.int$x)]) %>%
#   mutate(person_ID = as.factor(ord.prelim$model$person_ID),
#          video_name = as.factor(ord.prelim$model$video_name),
#          comfort_rating_ordered = ord.prelim$model$comfort_rating_ordered)
# 
# #         - scaling (put all on 0-1 scale by subtracting min and dividing by max) 
# 
# d.remodel.int2 = d.remodel.int2 %>% mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})

# -------------------------------------------------------------------------------------
#  RUN MODELS ON DYLAN'S DESKTOP  - SEE analysis_comfort.R for output of models and analysis of output ***----

# do on dylan's desktop:
# for (file in list.files("R", pattern = "null|int_per_|me_per_")) {
#   source(file.path("R",file))
# }

# (DEPRECATED scripts) 
# # Inclusive coefficient set-- source("R/run_models_1.R")
# # exclusive coefficient set: just top effects discovered before -- ource("R/run_models_2.R")
# # exclusive coefficient set + interactions-source("R/run_models_3.R")

# -------------------------------------------------------------------------------------
# ------------------------------ xx. Scratch -----------------------------------------
#       Explore housing cost (most missing data of variables in prelim models) ----

# Visualize relationship between housing cost and primary role
# definitely a pattern: dist moves expands right from student to grad to visiting (probably many postdocs) to staff to faculty
ggplot(d, aes(x = monthly_housing_cost)) + 
  geom_histogram() + 
  facet_wrap(~primary_role, scales = "free_y", ncol = 1)

# outliers
View(filter(d, monthly_housing_cost >= 5000)) 

colSums(apply(d, 2, is.na))
colSums(apply(d.model, 2, is.na))
table(rowSums(apply(d.model, 2, is.na)))

#hard to predict housing costs
summary(lm(monthly_housing_cost_class ~ ., data = d.model))

sort(colSums(apply(d.person, 2, is.na)))

lm.housing.cost = lm(as.numeric(monthly_housing_cost) ~ ., data = d.person %>% 
                       select(-"person_ID") %>%
                       select(-"monthly_housing_cost_class") %>%
                       # has small cells, use _BIKE version instead:
                       select(-c("secondary_mode")) %>%
                       # not significant, want more DF
                       select(-c("op_commute_best_imagine", "op_satisfied_with_commute", 
                                 "op_commute_goes_well", "op_like_driving", "op_travel_wasted",
                                 "op_eco_concern", "op_need_car", "op_commute_no_change",
                                 "op_limit_driving", "op_smartphone", "secondary_mode_BIKE"))
                       ) 

summary(lm.housing.cost)

#     binary outcome ----

# Use comfort_rating_3lev and remove neutrals
lm.binary.1 = glm(as.numeric(comfort_rating_3lev) - 1 ~ .,
                  data = d.model %>% mutate(comfort_rating_3lev = d$comfort_rating_3lev) %>% 
                    select(-c("comfort_rating", "comfort_rating_ordered")) %>%
                    filter(comfort_rating_3lev !="neutral") %>% droplevels, family = "binomial")

summary(lm.binary.1)



#     Ordered logit with person random effects (implemenation doesn't work, ignore) ####

#library("ordinal")

#d.model.randord =  d.model %>% select(-"comfort_rating") %>% mutate(person_ID = as.factor(d$person_ID))
#names(d.model.randord)

# randord.prelim = clmm2(comfort_rating_ordered ~ ., random=person_ID, data = d.model.randord)

# doesn't work:
# (and attempt with binary response got same error)
# randord.prelim = clmm2(comfort_rating_ordered ~ ., random=person_ID, data = d.model.randord)
# Error in setStart(rho) : attempt to find suitable starting values failed
#In addition: Warning messages:
#  1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 