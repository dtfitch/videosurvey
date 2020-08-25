library(brms)
d.remodel.me <- readRDS("data_for_models.RDS")
nchains = 3
# 
# Main effects + int vary by person + int vary by video + video group vary by video  ----
#   add: interactions of age and comfort_gour_no_lane3 with a few road variables 
fit3 <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
              age:bike_operating_space_ST +
              age:speed_limit_mph_ST_3lev.40.50. + 
              age:speed_prevail_minus_limit_ST + 
              age:veh_vol_non0_opspace_0_ST +
              comfort_four_no_lane3:street_parking_ST1 + 
              comfort_four_no_lane3:outside_lane_width_ft_ST + 
              comfort_four_no_lane3:bike_operating_space_ST,
            data=d.remodel.me,
            family=acat("logit"), iter = 2000,
            chains = nchains,
            cores = nchains,
            control = list(adapt_delta = 0.9, max_treedepth = 16),
            prior = c(set_prior("normal(0,0.5)", class = "b"),
                      set_prior("normal(0,1.5)", class = "Intercept"),
                      set_prior("student_t(3,0,0.5)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")))
fit3 <- add_criterion(fit3,"loo")
saveRDS(fit3, "fit3_acat.RDS")

fit3_cs_thresh <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (cs(1)  |video_name) + (VideoGroupWithin |video_name) - person_ID - video_name +
              age:bike_operating_space_ST +
              age:speed_limit_mph_ST_3lev.40.50. + 
              age:speed_prevail_minus_limit_ST + 
              age:veh_vol_non0_opspace_0_ST +
              comfort_four_no_lane3:street_parking_ST1 + 
              comfort_four_no_lane3:outside_lane_width_ft_ST + 
              comfort_four_no_lane3:bike_operating_space_ST,
            data=d.remodel.me,
            family=acat("logit"), iter = 2000,
            chains = nchains,
            cores = nchains,
            control = list(adapt_delta = 0.9, max_treedepth = 16),
            prior = c(set_prior("normal(0,0.5)", class = "b"),
                      set_prior("normal(0,1.5)", class = "Intercept"),
                      set_prior("student_t(3,0,0.5)", class = "sd"),
                      set_prior("lkj(2)", class = "cor")))
fit3_cs_thresh  <- add_criterion(fit3_cs_thresh,"loo")
saveRDS(fit3_cs_thresh , "fit3_acat_cs_thresh_video_name.RDS")