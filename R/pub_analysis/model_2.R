library(brms)
d.remodel.me <- readRDS("data_for_models.RDS")
nchains = 3
# 
# Main effects + int vary by person + int vary by video + video group vary by video  ----
#   add: interactions of age and comfort_gour_no_lane3 with a few road variables 
fit2 <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
            age:bike_operating_space_ST +
            age:speed_limit_mph_ST_3lev.40.50. + 
            age:speed_prevail_minus_limit_ST + 
            age:veh_vol_non0_opspace_0_ST +
            comfort_four_no_lane3:street_parking_ST1 + 
            comfort_four_no_lane3:outside_lane_width_ft_ST + 
            comfort_four_no_lane3:bike_operating_space_ST,
                 data=d.remodel.me,
                 family=cumulative("logit"), iter = 2000,
                 chains = nchains,
                 cores = nchains,
                 control = list(adapt_delta = 0.9, max_treedepth = 16),
                 prior = c(set_prior("normal(0,0.5)", class = "b"),
                    set_prior("normal(0,1.5)", class = "Intercept"),
                    set_prior("student_t(3,0,0.5)", class = "sd"),
                    set_prior("lkj(2)", class = "cor")))
fit2 <- add_criterion(fit2,"loo")
saveRDS(fit2, "fit2.RDS")

# EDA below to determine which interations to include. Also see evaluate_interactions.R---------------

# Main effects + int vary by person + int vary by video + video group vary by video  ---- 
#   add: interactions between person vars and road vars
# fit <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
#            age:street_parking_ST1 + age:outside_lane_width_ft_ST + age:veh_volume2_ST2 +
#            age:veh_volume2_ST3 + age:bike_operating_space_ST + age:speed_limit_mph_ST_3lev.30.40. +
#            age:speed_limit_mph_ST_3lev.40.50. + age:bike_lane_SUM_ST1 + age:bike_lane_SUM_ST1 + 
#            age:speed_prevail_minus_limit_ST + age:veh_vol_non0_opspace_0_ST, 
#                  data=d.remodel.me, 
#                  family=cumulative("logit"), iter = 2000,
#                  chains = nchains,
#                  cores = nchains,
#                  control = list(adapt_delta = 0.9, max_treedepth = 16),
#                  prior = c(set_prior("normal(0,0.5)", class = "b"), 
#                     set_prior("normal(0,1.5)", class = "Intercept"),
#                     set_prior("student_t(3,0,0.5)", class = "sd"),
#                     set_prior("lkj(2)", class = "cor")))
# 
# saveRDS(fit, "fit2_age.RDS")

# fit <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
#              bike_ability:street_parking_ST1 + bike_ability:outside_lane_width_ft_ST + bike_ability:veh_volume2_ST2 +
#              bike_ability:veh_volume2_ST3 + bike_ability:bike_operating_space_ST + bike_ability:speed_limit_mph_ST_3lev.30.40. +
#              bike_ability:speed_limit_mph_ST_3lev.40.50. + bike_ability:bike_lane_SUM_ST1 + bike_ability:bike_lane_SUM_ST1 + 
#              bike_ability:speed_prevail_minus_limit_ST + bike_ability:veh_vol_non0_opspace_0_ST, 
#            data=d.remodel.me, 
#            family=cumulative("logit"), iter = 2000,
#            chains = nchains,
#            cores = nchains,
#            control = list(adapt_delta = 0.9, max_treedepth = 16),
#            prior = c(set_prior("normal(0,0.5)", class = "b"), 
#                      set_prior("normal(0,1.5)", class = "Intercept"),
#                      set_prior("student_t(3,0,0.5)", class = "sd"),
#                      set_prior("lkj(2)", class = "cor")))
# 
# saveRDS(fit, "fit2_bike_ability.RDS")

# fit <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
#              comfort_four_no_lane2:street_parking_ST1 + comfort_four_no_lane2:outside_lane_width_ft_ST + comfort_four_no_lane2:veh_volume2_ST2 +
#              comfort_four_no_lane2:veh_volume2_ST3 + comfort_four_no_lane2:bike_operating_space_ST + comfort_four_no_lane2:speed_limit_mph_ST_3lev.30.40. +
#              comfort_four_no_lane2:speed_limit_mph_ST_3lev.40.50. + comfort_four_no_lane2:bike_lane_SUM_ST1 + comfort_four_no_lane2:bike_lane_SUM_ST1 + 
#              comfort_four_no_lane2:speed_prevail_minus_limit_ST + comfort_four_no_lane2:veh_vol_non0_opspace_0_ST, 
#            data=d.remodel.me, 
#            family=cumulative("logit"), iter = 2000,
#            chains = nchains,
#            cores = nchains,
#            control = list(adapt_delta = 0.9, max_treedepth = 16),
#            prior = c(set_prior("normal(0,0.5)", class = "b"), 
#                      set_prior("normal(0,1.5)", class = "Intercept"),
#                      set_prior("student_t(3,0,0.5)", class = "sd"),
#                      set_prior("lkj(2)", class = "cor")))
# 
# saveRDS(fit, "fit2_comfort_four_no_lane2.RDS")

# fit <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
#              comfort_four_no_lane3:street_parking_ST1 + comfort_four_no_lane3:outside_lane_width_ft_ST + comfort_four_no_lane3:veh_volume2_ST2 +
#              comfort_four_no_lane3:veh_volume2_ST3 + comfort_four_no_lane3:bike_operating_space_ST + comfort_four_no_lane3:speed_limit_mph_ST_3lev.30.40. +
#              comfort_four_no_lane3:speed_limit_mph_ST_3lev.40.50. + comfort_four_no_lane3:bike_lane_SUM_ST1 + comfort_four_no_lane3:bike_lane_SUM_ST1 + 
#              comfort_four_no_lane3:speed_prevail_minus_limit_ST + comfort_four_no_lane3:veh_vol_non0_opspace_0_ST, 
#            data=d.remodel.me, 
#            family=cumulative("logit"), iter = 2000,
#            chains = nchains,
#            cores = nchains,
#            control = list(adapt_delta = 0.9, max_treedepth = 16),
#            prior = c(set_prior("normal(0,0.5)", class = "b"), 
#                      set_prior("normal(0,1.5)", class = "Intercept"),
#                      set_prior("student_t(3,0,0.5)", class = "sd"),
#                      set_prior("lkj(2)", class = "cor")))
# fit <- add_criterion(fit,"loo")
# saveRDS(fit, "fit2_comfort_four_no_lane3.RDS")

# fit <- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name +
#              usual_mode_4levBike:street_parking_ST1 + usual_mode_4levBike:outside_lane_width_ft_ST + usual_mode_4levBike:veh_volume2_ST2 +
#              usual_mode_4levBike:veh_volume2_ST3 + usual_mode_4levBike:bike_operating_space_ST + usual_mode_4levBike:speed_limit_mph_ST_3lev.30.40. +
#              usual_mode_4levBike:speed_limit_mph_ST_3lev.40.50. + usual_mode_4levBike:bike_lane_SUM_ST1 + usual_mode_4levBike:bike_lane_SUM_ST1 + 
#              usual_mode_4levBike:speed_prevail_minus_limit_ST + usual_mode_4levBike:veh_vol_non0_opspace_0_ST, 
#            data=d.remodel.me, 
#            family=cumulative("logit"), iter = 2000,
#            chains = nchains,
#            cores = nchains,
#            control = list(adapt_delta = 0.9, max_treedepth = 16),
#            prior = c(set_prior("normal(0,0.5)", class = "b"), 
#                      set_prior("normal(0,1.5)", class = "Intercept"),
#                      set_prior("student_t(3,0,0.5)", class = "sd"),
#                      set_prior("lkj(2)", class = "cor")))
# fit <- add_criterion(fit,"loo")
# saveRDS(fit, "fit2_usual_mode_4levBike.RDS")