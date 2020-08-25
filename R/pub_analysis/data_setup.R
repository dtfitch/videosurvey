# revised setup script for new models
# just needed to add VideoGroup

source("data_comfort.R")
#source("eda_comfort.R")

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
# Protected bike lane implies buffered implies bike lane
# Only one street has a proected bike lane so I didn't make it it's own level
d$bike_lane_SUM_ST = as.factor(d$bike_lane_ST + d$buffer_ST)


d$speed_prevail_minus_limit_ST = d$prevailing_speed_mph_ST - d$speed_limit_mph_ST

#   Make new d.model for modeling ----

d.model = d %>% dplyr::select(-c("cts_ID", "ID", "URL",
                          "block_ID", "sub_ID", #<- might want these later
                          "video_name")) %>%
  # hh18_older has weird entries and overlaps with household type anyway.
  # hh_composition is also weirdly coded. I recoded to 4-level.
  # housing_unit_type also has lots of missing data   
  # rent_split is similar to rent_share but more missing data
  # keep rent share (more descriptive with less missing data)
  # edu_self has too-high correlation with primary role. edu_parent has lots of missing data
  dplyr::select(-c("hh18_older", "hh_composition", "housing_unit_type",
            "rent_split", 
            "edu_self", "edu_parent")) %>%
  # Only two blocked ones and I looked at the videos -- neither actually seems "blocked"
  dplyr::select(-c("bike_lane_blocked_ST")) %>%
  # removed bike_speed_mph_ST bc of data quality question (see top)
  dplyr::select(-c("bike_speed_mph_ST")) %>%
  # bike_operating_space_ST is just a tracking variable
  # dplyr::select(-c("bike_operating_space_ST")) %>%
  # Removed in favor of other version or similar var
  dplyr::select(-c("op_like_biking_3lev", "comfort_rating_3lev",
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
  dplyr::select(-c("NCHRP_BLOS_score_ST", "LTS_ST")) %>%
  # Summed variables describing liking commute instead
  dplyr::select(-contains("commute", ignore.case = F)) %>%
  # Too many levels (may add vack in later)
  # Reduce levels of seconary mode?
  # Reduce levels of child vars to just have or don't have kid? (lots of missing data tho)
  dplyr::select(-c("child6", "child615", "child1617")) %>%
  # unlikely relationship (may add back in later)
  dplyr::select(-c("distance", "op_smartphone", "license")) %>%
  # removed after preliminary models and no strong reason to keep them (may add vack in later)
  dplyr::select(-c(#"divided_road_ST",
    "op_eco_concern", "bike_access", 
    "op_travel_wasted", "op_like_driving",
    "op_schedule_transit", "op_limit_driving", "op_need_own_car",
     "secondary_mode_BIKE",
    "op_bike_often", # mostly captured by bike commuting
    "hh_composition_4lev",
    "urban_ST",
    "rent_share")) %>%
  # make orderd vars numeric to reduce variables in model (can revert or re-bin if very non-linear)
  mutate_if(is.ordered, as.numeric) %>%
  mutate(comfort_rating_ordered = d$comfort_rating)

names(d.model)



d.remodel.me = d.model %>%
  dplyr::select(-"comfort_rating") %>%
  dplyr::select(-c("num_lanes_ST", "primary_role", "divided_road_ST",
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

d.remodel.me = d.remodel.me %>% dplyr::select(-c("X.Intercept.", "femaleNA","usual_mode_4levCar", "usual_mode_4levPublic.Trans"))
str(d.remodel.me)

#         - scaling (put all on 0-1 scale by subtracting min and dividing by max)

d.remodel.me = d.remodel.me %>% mutate_if(is.numeric, function(x) {(x-min(x, na.rm = T))/max(x - min(x, na.rm = T), na.rm = T)})
str(d.remodel.me)

saveRDS(d.remodel.me,"./pub_analysis/data_for_models.RDS")
