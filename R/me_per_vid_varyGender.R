nchains = 3
load("to_5i_jul_26.RData")
library(brms)


# Main effects + person + video random effects -- 
me_per_vid_varyGender = brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + female1 |video_name) - person_ID - video_name, 
                         data=d.remodel.me, 
                         family=cumulative("logit"), iter = 2000,
                         chains = nchains,
                         cores = nchains,
                         control = list(adapt_delta = 0.9, max_treedepth = 16),
                         prior = c(set_prior("normal(0,5)", class = "b"), 
                                   set_prior("student_t(3,0,5)", class = "Intercept"),
                                   set_prior("student_t(3,0,5)", class = "sd"),
                                   set_prior("lkj(2)", class = "cor"))
)  

saveRDS(me_per_vid_varyGender, "me_per_vid_varyGender.RDS")

# Main effects + person + video random effects -- 
int_per_byGender = brm(comfort_rating_ordered ~ . + female1:street_parking_ST1 +
                             female1:street_parking_ST1 +
                             female1:outside_lane_width_ft_ST +
                             female1:veh_volume2_ST2 +
                             female1:veh_volume2_ST3 +
                             female1:bike_operating_space_ST +
                             female1:usual_mode_4levBike +
                             female1:speed_limit_mph_ST_3lev.30.40. +
                             female1:speed_limit_mph_ST_3lev.40.50. +
                             female1:bike_lane_SUM_ST1 +
                             female1:bike_lane_SUM_ST2 +
                             female1:speed_prevail_minus_limit_ST +
                             female1:veh_vol_non0_opspace_0_ST +
                             (1|person_ID) - person_ID - video_name, 
                            data=d.remodel.me, 
                            family=cumulative("logit"), iter = 2000,
                            chains = nchains,
                            cores = nchains,
                            control = list(adapt_delta = 0.9, max_treedepth = 16),
                            prior = c(set_prior("normal(0,5)", class = "b"), 
                                      set_prior("student_t(3,0,5)", class = "Intercept"),
                                      set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(int_per_byGender, "int_per_byGender.RDS")
