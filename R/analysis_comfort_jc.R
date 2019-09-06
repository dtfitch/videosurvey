# Load and analyse output of models created in model_comfort_jc.R -----
# 
# Notes:
# Have main and int, with and without video re, two nulls <- 6 total models
# (policy would say) ~5 ft for bike lane, ~7ft for protected bike land, bufferred?
#
# TO DO: 
#
# See jane activites
# pp_check, ppc_bars , e..g. pp_check(null2.loo, type = "bars", nsamples = "100")
# brms tools forest for random effects
# brmstools for coefficients?
#
# ---------------------------------------------------------------------------------------------------------------

# 0. Setup ---- 
path.to.models = "/Users/jac/Box/Video_CTS/Github/videosurvey/R/"
setwd(path.to.models)

library(brms)
library(ggplot2)
library(ggridges)
library(bayesplot)
library(rstan)
library(dplyr)
library(cowplot)
library(glmnetcr)
library(forcats)
library(stringr)
library(reshape2)
#also should have plyr and installed

load("to_5i_jul_26.RData")


# - Model loads ----

models = readRDS("models_with_loo.RDS")
names(models) = sapply(sapply(names(models), strsplit, "\\."), first)

int_per.array = as.array(models$int_per)
int_per_vid.array = as.array(models$int_per_vid)
me_per.array = as.array(models$me_per)
me_per_vid.array = as.array(models$me_per_vid)

null_per.array = as.array(models$null_per)
null_per_vid.array = as.array(models$null_per_vid)

null_loo_reduced_data = readRDS("null_loo_reduced_data.RDS")


# - Make variable name dictionary ----

varname_dict = names(models[["me_per_vid"]]$data)
names(varname_dict) = varname_dict

tmp.rename.func <- function(varname_dict, var, newname) {
  x = which(varname_dict == var)
  if (length(x) > 0) {varname_dict[x] =  newname}
  varname_dict
}

varname_dict = tmp.rename.func(varname_dict, "comfort_rating_ordered", "comfort rating")

varname_dict = tmp.rename.func(varname_dict, "female1", "IND female")
varname_dict = tmp.rename.func(varname_dict, "child_u18TRUE", "IND household has child under 18")
varname_dict = tmp.rename.func(varname_dict, "age_impute", "IND age, imputed")
varname_dict = tmp.rename.func(varname_dict, "primary_role", "IND primary role")

varname_dict = tmp.rename.func(varname_dict, "op_like_biking", "IND like riding a bike")
varname_dict = tmp.rename.func(varname_dict, "op_need_car", "IND need car for activities")
varname_dict = tmp.rename.func(varname_dict, "op_feel_safe", "IND feel safe biking on campus")
varname_dict = tmp.rename.func(varname_dict, "op_like_transit", "IND like using public transit")
varname_dict = tmp.rename.func(varname_dict, "op_arrive_professional", "IND job needs professional attire")
varname_dict = tmp.rename.func(varname_dict, "op_travel_stress", "IND travelling to campus is stressful")
varname_dict = tmp.rename.func(varname_dict, "bike_ability", "IND confidence level riding a bike")
varname_dict = tmp.rename.func(varname_dict, "comfort_four_no_lane2", "IND willing to bike on 4-lane road")
varname_dict = tmp.rename.func(varname_dict, "comfort_four_no_lane3", "IND comfortable biking on 4-lane road")
varname_dict = tmp.rename.func(varname_dict, "usual_mode_4levBike", "IND usually commute to campus by bike")

varname_dict = tmp.rename.func(varname_dict, "street_parking_ST1", "ST street parking")
varname_dict = tmp.rename.func(varname_dict, "outside_lane_width_ft_ST", "ST outside lane width")
varname_dict = tmp.rename.func(varname_dict, "veh_volume2_ST2", "ST low volume")
varname_dict = tmp.rename.func(varname_dict, "veh_volume2_ST3", "ST high volume")
varname_dict = tmp.rename.func(varname_dict, "bike_operating_space_ST", "ST bike operating space")
varname_dict = tmp.rename.func(varname_dict, "bike_lane_SUM_ST1", "ST bike lane, no buffer")
varname_dict = tmp.rename.func(varname_dict, "bike_lane_SUM_ST2", "ST bike lane, with buffer")
varname_dict = tmp.rename.func(varname_dict, "speed_prevail_minus_limit_ST", "ST prevailing minus posted speed")
varname_dict = tmp.rename.func(varname_dict, "speed_limit_mph_ST_3lev.30.40.", "ST speed limit [30,40)")
varname_dict = tmp.rename.func(varname_dict, "speed_limit_mph_ST_3lev.40.50.", "ST speed limit [40,50]")
varname_dict = tmp.rename.func(varname_dict, "veh_vol_non0_opspace_0_ST", "ST bikes share space with cars")

varname_dict = tmp.rename.func(varname_dict, "person_ID", "person ID")
varname_dict = tmp.rename.func(varname_dict, "video_name", "video name")

#more we'll need below
varname_dict2 = setNames(nm = c("ability_comfort", "road_environment", "id", "attitude",
                                "b_Intercept\\[1\\]", "b_Intercept\\[2\\]", "b_Intercept\\[3\\]",
                                "b_Intercept\\[4\\]", "b_Intercept\\[5\\]", "b_Intercept\\[6\\]",
                                "sd_person_ID__Intercept", "sd_video_name__Intercept"),
                         c("biking comfort", "road environment", "id", "transit attitudes",
                            "Intercept 1 ", "Intercept 2", "Intercept 3",
                           "Intercept 4", "Intercept 5", "Intercept 6",
                           "SD person ID Intercept", "SD video name Intercept"))

varname_dict = c(varname_dict, varname_dict2)
rm(varname_dict2)

saveRDS(varname_dict, file = "../data/variable_name_dictionary.RDS")

# 1. Model diagnostics and summary ----
    # 
    #   - Compare loo ----
    
    loo_compare("me_per" = models$me_per$loo, 
                "me_per_vid" = models$me_per_vid$loo, 
                "int_per" =models$int_per$loo, 
                "int_per_vid" = models$int_per_vid$loo, 
                null_loo_reduced_data$null_per_loo2,
                null_loo_reduced_data$null_per_vid_loo2)

# elpd_diff se_diff
# me_per_vid                      0.0       0.0
# int_per_vid                    -0.2       1.9
# readRDS("null_per_vid.RDS")  -126.1      17.3
# int_per                      -236.8      22.3
# me_per                       -493.4      31.4
# readRDS("null_per.RDS")     -2933.4      72.8

    #   - Quick summary ----
    summary(models$int_per)
    
    #   - Diagnostics ----
    sapply(models, function(x) {check_hmc_diagnostics(x$fit)}) #all look good
    
# 2. Model coefficients and random effects ----
    
    # coefficients ----

    #intervals
    mcmc_intervals(me_per_vid.array, 
                   pars = dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters,
                   pattern = "person_ID|lp__|r_person|r_video")], prob = .5, prob_outer = .95) + 
      geom_vline(aes(xintercept = 0))

    #density
    # I made a custom version of mcmc_ares_ridges in the mcmc_areas_ridges2.R file
    # It uses a "size" argument to control the thickness of the lines in the density plots
    # "names" maps the parameter coefficient names to readable names
  source("mcmc_areas_ridges2.R")


    mcmc_areas_ridges2(me_per_vid.array, 
                      pars = (dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters,
                      pattern = "person_ID\\[|lp__|r_person|r_video")]), prob = .99, prob_outer = .99,
                     size = .1, names = varname_dict) + 
      geom_vline(aes(xintercept = 0))  + theme_bw() 
    
    ggsave(filename = "../IMG/me_per_vid_ridges.png", width = 6.5, height = 5)
    
    
    mcmc_areas_ridges2(int_per_vid.array, 
                      pars = dimnames(int_per_vid.array)$parameters[!grepl(dimnames(int_per_vid.array)$parameters, 
                      pattern = "person_ID\\[|lp__|r_person|r_video")], prob = .99, prob_outer = .99,
                      size = .1, names = varname_dict) + 
      geom_vline(aes(xintercept = 0)) + theme_bw()
    
    ggsave(filename = "../IMG/int_per_vid_ridges.png", width = 6.5, height = 5)
    
    
    mcmc_areas_ridges2(me_per.array, 
                      pars = dimnames(me_per.array)$parameters[!grepl(dimnames(me_per.array)$parameters, 
                       pattern = "person_ID\\[|lp__|r_person|r_video")], prob = .99, prob_outer = .99,
                      size = .1, names = varname_dict) + 
      geom_vline(aes(xintercept = 0)) + theme_bw()
    
    ggsave(filename = "../IMG/me_per_ridges.png", width = 6.5, height = 5)
    
    
    mcmc_areas_ridges2(int_per.array, 
                      pars = dimnames(int_per.array)$parameters[!grepl(dimnames(int_per.array)$parameters, 
                       pattern = "person_ID\\[|lp__|r_person|r_video")], prob = .99, prob_outer = .99,
                      size = .1, names = varname_dict) + 
      geom_vline(aes(xintercept = 0)) + theme_bw()
  
    ggsave(filename = "../IMG/int_per_ridges.png", width = 6.5, height = 5)
    
    
    # Compare two model coefficients
    
    model2.array = as.array(models$int_per_vid)
    
    # coefficients without video random effects
    plot_grid(
    mcmc_intervals(me_per.array, pars = dimnames(me_per.array)$parameters[!grepl(dimnames(me_per.array)$parameters, pattern = "person_ID|lp__|r_person|r_video")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0)),
    mcmc_intervals(int_per.array, pars = dimnames(int_per.array)$parameters[!grepl(dimnames(int_per.array)$parameters, pattern = "person_ID|lp__|r_person|r_video|hs_c2")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0)), 
    mcmc_intervals(me_per_vid.array, pars = dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters, pattern = "person_ID|lp__|r_person|r_video|hs_c2")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0)),
    mcmc_intervals(int_per_vid.array, pars = dimnames(int_per_vid.array)$parameters[!grepl(dimnames(int_per_vid.array)$parameters, pattern = "person_ID|lp__|r_person|r_video|hs_c2")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0)), nrow = 2
    )
    
    # random effects ----
    
    
    # just video random effects
    plot_grid(
    mcmc_intervals(me_per_vid.array, pars = dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0)),
    mcmc_intervals(int_per_vid.array, pars = dimnames(int_per_vid.array)$parameters[!grepl(dimnames(int_per_vid.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
      geom_vline(aes(xintercept = 0))
    )

    
    # Look at videos with large random effects
    View(d %>% group_by(video_name) %>% 
           summarize(first(URL), first(veh_volume2_ST),first(bike_lane_SUM_ST),
                     first(bike_operating_space_ST), first(speed_limit_mph_ST), first(bike_speed_mph_ST)))
    
    
    # Many large person-level random effects -- do we want a more restrictive prior?
    # example with me_per
    pi.samples = posterior_samples(models$me_per, pars = "person_ID")
    pi.means = apply(pi.samples, 2, mean)
    hist(pi.means); summary(pi.means)
    hist(apply(pi.samples, 2, median), breaks = 20)
    summary(apply(pi.samples, 2, median))
    
    
    # Look at inidividaul large person random effects
    # Especially for people who always feel uncomfortable despite a large bike lane
    which(pi.means < -5 )
    filter(d, person_ID %in% c("21", "2149", "1587", "2060", "44")) %>% select(comfort_rating, NCHRP_BLOS_ST, bike_lane_SUM_ST, comfort_four_no_lane, video_name, VideoGroup, person_ID) %>%
      arrange(person_ID)
    #weird that they all have the same videos?
    
    
    # Look at distributions of random effects when large
    length(which(abs(pi.means) > 3 ))
    pi.samples.melt = reshape2::melt(data.frame(pi.samples[,which(abs(pi.means) > 3 )]))
    pi.samples.melt$variable = factor(pi.samples.melt$variable,
                                      levels = unique(pi.samples.melt$variable)[order(pi.means[which(abs(pi.means) > 3 )])])
    ggplot(data = pi.samples.melt) + 
      ggridges::geom_density_ridges(aes(x = value, group = variable, y = variable))
    
    
    
# 3. Scenarios to understand coefficients ----
    
    # summary 
    
    str(models$me_per$data)
    names(models$me_per$data)
    
    # create by-person data frame for finding quantiles accurately
    d.scenario = models$me_per$data %>% group_by(person_ID) %>% select(-"comfort_rating_ordered") %>% summarize_all(first)
    
    #   Building blocks ----
    #     - Individual-level ----
    
    #       + attitudes ----
    
    op_levels = data.frame(apply(d.scenario[,-1], 2, quantile, probs = c(.1,.5,.9)))
    
    low_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[1], op_feel_safe = op_levels$op_feel_safe[1], op_like_transit = op_levels$op_like_transit[1])
    mid_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[2], op_feel_safe = op_levels$op_feel_safe[2], op_like_transit = op_levels$op_like_transit[2])
    high_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[3], op_feel_safe = op_levels$op_feel_safe[3], op_like_transit = op_levels$op_like_transit[3])
    
    low_neg_attitudes = c(op_need_car = op_levels$op_need_car[1], op_arrive_professional = op_levels$op_arrive_professional[1], op_travel_stress = op_levels$op_travel_stress[1])
    mid_neg_attitudes = c(op_need_car = op_levels$op_need_car[2], op_arrive_professional = op_levels$op_arrive_professional[2], op_travel_stress = op_levels$op_travel_stress[2])
    high_neg_attitudes = c(op_need_car = op_levels$op_need_car[3], op_arrive_professional = op_levels$op_arrive_professional[3], op_travel_stress = op_levels$op_travel_stress[3])
    
    bad_attidudes = c(low_pos_attitudes, high_neg_attitudes)
    mid_attitudes = c(mid_pos_attitudes, mid_neg_attitudes)
    good_attidudes = c(high_pos_attitudes, low_neg_attitudes)
    
    #       + ability + comfort ----
    
    low_ability_comfort = data.frame(comfort_four_no_lane2=0, comfort_four_no_lane3=0, bike_ability=.5, usual_mode_4levBike = 0) #somewhat confident, low comfort      
    mid_ability_comfort = data.frame(comfort_four_no_lane2=1, comfort_four_no_lane3=0, bike_ability=.5, usual_mode_4levBike = 0) #somewaht confident, moderate comfort
    high_ability_comfort = data.frame(comfort_four_no_lane2=0, comfort_four_no_lane3=1, bike_ability=1, usual_mode_4levBike = 1) #very confident, high comfort    
    
    #       + demographic ----
    range(d.person$age_impute)
    agelevels = quantile(d.scenario$age_impute , c(.1,.8,.95), na.rm = T)
    # On real scale
    agelevels*(max(d.model$age_impute) - min(d.model$age_impute)) + min(d.model$age_impute)
    # 10% 80% 95% 
    # 20  34  57
    young_childless_male = data.frame(age_impute = agelevels[1], child_u18TRUE = 0, female1 = 0)
    midage_child_female = data.frame(age_impute = agelevels[2], child_u18TRUE = 1, female1 = 1)
    old_childless_male = data.frame(age_impute = agelevels[3], child_u18TRUE = 0, female1 = 0)
    old_childless_female = data.frame(age_impute = agelevels[3], child_u18TRUE = 0, female1 = 1)
    
    #     - Road environment ----
    
    speed_prevail_levels = quantile(models$me_per$data$speed_prevail_minus_limit_ST, c(.05,.5,.95))
    speed_prevail_levels*(max(d.model$speed_prevail_minus_limit_ST) - min(d.model$speed_prevail_minus_limit_ST)) + min(d.model$speed_prevail_minus_limit_ST)
    # 5% 50% 95% 
    # -10   0   5  
    
    outside_lane_levels = quantile(models$me_per$data$outside_lane_width_ft_ST, c(.05,.5,.95))
    outside_lane_levels*(max(d.model$outside_lane_width_ft_ST) - min(d.model$outside_lane_width_ft_ST)) + min(d.model$outside_lane_width_ft_ST)
    # 5% 50% 95% 
    # 9  11  13 
    
    bike_space_levels = quantile(models$me_per$data$bike_operating_space_ST, c(.05,.5,.95))
    bike_space_levels*(max(d.model$bike_operating_space_ST) - min(d.model$bike_operating_space_ST)) + min(d.model$bike_operating_space_ST)
    # 5% 50% 95% 
    # 0   5  11 
    
    
    collector_good = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 0,
                                speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 0,
                                bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 1, 
                                speed_prevail_minus_limit_ST = speed_prevail_levels[1],  
                                street_parking_ST1 = 1,
                                outside_lane_width_ft_ST = outside_lane_levels[1],
                                bike_operating_space_ST = bike_space_levels[3], 
                                veh_vol_non0_opspace_0_ST = 0) 
    
    collector_mid = data.frame(veh_volume2_ST2 = 1, veh_volume2_ST3 = 0,
                               speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                               bike_lane_SUM_ST1 = 1, bike_lane_SUM_ST2 = 0,
                               speed_prevail_minus_limit_ST = speed_prevail_levels[2],  
                               street_parking_ST1 = 1,
                               outside_lane_width_ft_ST = outside_lane_levels[2],
                               bike_operating_space_ST = bike_space_levels[2], 
                               veh_vol_non0_opspace_0_ST = 0) 
    
    collector_bad = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                               speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                               bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 0,
                               speed_prevail_minus_limit_ST = speed_prevail_levels[3],  
                               street_parking_ST1 = 1,
                               outside_lane_width_ft_ST = outside_lane_levels[3],
                               bike_operating_space_ST = bike_space_levels[1], 
                               veh_vol_non0_opspace_0_ST = 1) 
    
    
    arterial_good = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                                speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                                bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 1,
                                speed_prevail_minus_limit_ST = speed_prevail_levels[1],  
                                street_parking_ST1 = 1, # all streets have street parking
                                outside_lane_width_ft_ST = outside_lane_levels[1], #<- outside lane width effect is neg
                                bike_operating_space_ST = bike_space_levels[3], 
                                veh_vol_non0_opspace_0_ST = 0) 
    
    arterial_mid = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                               speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 1,
                               bike_lane_SUM_ST1 = 1, bike_lane_SUM_ST2 = 0, 
                               speed_prevail_minus_limit_ST = speed_prevail_levels[2],  
                               street_parking_ST1 = 1, # all streets have street parking
                               outside_lane_width_ft_ST = outside_lane_levels[2],
                               bike_operating_space_ST = bike_space_levels[2], 
                               veh_vol_non0_opspace_0_ST = 0) 
    
    arterial_bad = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                               speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 1,
                               bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 0, 
                               speed_prevail_minus_limit_ST = speed_prevail_levels[3],  
                               street_parking_ST1 = 1, # all streets have street parking
                               outside_lane_width_ft_ST = outside_lane_levels[3],
                               bike_operating_space_ST = bike_space_levels[1], 
                               veh_vol_non0_opspace_0_ST = 1) 
    
    
    
    
    #   Counterfactuals (makes "all_counterfactuals") ----
    
    #     - Create entry options (same for all models) ----
    
attitudes = data.frame(rbind(bad_attidudes, mid_attitudes, good_attidudes), 
                        id = 1,  attitude = c("bad_attidude", "mid_attitude", "good_attidude"))
attitudes$attitude = ordered(attitudes$attitude, levels(attitudes$attitude)[c(1,3,2)])

ability_comfort = data.frame(rbind(low_ability_comfort, mid_ability_comfort, high_ability_comfort), 
                        id = 1,  ability_comfort = c("low_comfort", "mid_comfort", "high_comfort"))
ability_comfort$ability_comfort = ordered(ability_comfort$ability_comfort, levels(ability_comfort$ability_comfort)[c(2,3,1)])

road_environments = data.frame(rbind(collector_bad, collector_mid, collector_good,
                                     arterial_bad, arterial_mid, arterial_good),
                        id = 1,  road_environment = c("collector_bad", "collector_mid", "collector_good",
                                        "arterial_bad", "arterial_mid", "arterial_good"))

road_environments$road_environment = ordered(road_environments$road_environment,
                levels = c("arterial_bad", "arterial_mid", "arterial_good", "collector_bad", "collector_mid", "collector_good"))

person = data.frame(rbind(young_childless_male, #midage_child_female, old_childless_female,
                          old_childless_male),
                        id = 1, person =  c("20yr_man", #"midage_child_female", "old_childless_male", 
                                          "57yr_woman"))

all_counterfactuals = plyr::join_all(list(attitudes, ability_comfort, road_environments, person), by='id', type='full', )
all_counterfactuals$rowID = 1:nrow(all_counterfactuals)

building_blocks = list(attitudes = attitudes, ability_comfort = ability_comfort, road_environments = road_environments)

sapply(building_blocks, function(x) {names(x) = varname_dict[names(x)]; x})

saveRDS(building_blocks, file = "../data/counterfactual_building_blocks.RDS")

    #       + Add interactions ----
interaction.terms = trimws(strsplit(as.character(models$int_per$formula[[1]][3][1]), " \\+ ")[[1]])
interaction.terms = interaction.terms[grepl(interaction.terms, pattern = "\\.[A-za-z]{2}")]

interactions = data.frame(do.call("cbind", lapply(interaction.terms, function(term) {
  tmp = all_counterfactuals %>% select(names(all_counterfactuals)[sapply(names(all_counterfactuals), grepl, x = term)])
  apply(tmp, 1, prod)
})))
names(interactions) = interaction.terms

all_counterfactuals = data.frame(all_counterfactuals, interactions)

    #   Get and save fitted values (model specific) ----

#example
x = data.frame(c(bad_attidudes, high_ability_comfort, young_childless_male, collector_bad)) #note this doesn't have a person_ID
predict(models$me_per, newdata = x, re_formula = NA) 
fitted(models$me_per, newdata = x, re_formula = NA)

# created fitted.names from example
fitted.names = as.vector(outer(colnames(fitted(models$me_per, newdata = x, re_formula = NA)),
                               colnames(fitted(models$me_per, newdata = x, re_formula = NA)[,,]), 
                               "paste"))

make_models_fitted = function(model_name) {
  
  cat("calculating for ", model_name)
  
  all_counterfactuals.fitted = sapply(1:nrow(all_counterfactuals), function(x) {
      fitted(models[[model_name]], newdata = all_counterfactuals[x,], re_formula = NA)
  })
  
  all_counterfactuals.fitted = data.frame(t(all_counterfactuals.fitted))
  names(all_counterfactuals.fitted) = fitted.names
  all_counterfactuals.fitted$rowID = 1:nrow(all_counterfactuals.fitted)
  all_counterfactuals.fitted.melt = reshape2::melt(all_counterfactuals.fitted, id.vars = "rowID")
  
  all_counterfactuals = full_join(all_counterfactuals, all_counterfactuals.fitted.melt, by = "rowID")
  all_counterfactuals = all_counterfactuals %>% mutate(class = interaction(attitude, ability_comfort, road_environment))
  all_counterfactuals$road_type =  sapply(as.character(all_counterfactuals$road_environment), "switch",
                                          arterial_bad = "arterial",
                                          arterial_mid = "arterial",
                                          arterial_good = "arterial",
                                          collector_bad = "collector",
                                          collector_mid = "collector",
                                          collector_good = "collector")    
  return(all_counterfactuals)
}

models_per.fitted = lapply(c("int_per", "me_per", "null_per"), make_models_fitted)
names(models_per.fitted) = c("int_per", "me_per", "null_per")

models_per_vid.fitted = lapply(c("int_per_vid", "me_per_vid", "null_per_vid"), make_models_fitted)
names(models_per_vid.fitted) = c("int_per_vid", "me_per_vid", "null_per_vid")

models_fitted = c(models_per.fitted, models_per_vid.fitted)

#saveRDS(models_fitted, "models_fitted.RDS")

    #     - Plot and save (model specific) ----

first.nonNA = function(x) {first(na.omit(x))}

sapply(1:length(models_fitted), function(i) {
  
  tmp.all_counterfactuals = models_fitted[[i]]
  model_name = names(models_fitted)[i]
  if (!model_name %in%  c("null_per", "null_per_vid")) {
    
  #   + plots by road type                        
  counterfactuals_by_road = split(tmp.all_counterfactuals, tmp.all_counterfactuals$road_type)

  #Collector
  
  ggplot( counterfactuals_by_road$collector %>% 
            filter(!grepl("Error", variable))  %>%
            #have to reshape to seperate out lower, estimate, and upper since I didn't before
            mutate( variable_type = fct_relabel(variable, str_extract, "[A-Za-z0-9\\.]*"),
                    variable_level = fct_relabel(variable, str_extract, paste(levels(d$comfort_rating),collapse="|")),
                    ) %>%
            tidyr::spread(variable_type,value) %>%
            group_by(class, person, variable_level) %>% summarize_all(first.nonNA)
  ) +
    geom_ribbon(stat = "identity", aes(x = as.factor(variable_level), ymin = Q2.5, ymax = Q97.5,
                                         group = person, 
                                         fill = person,
                                         color = person), linetype = 3, alpha = .3) +
    geom_line(aes(x = as.factor(variable_level), y = Estimate,
                  group = person,
                  color = person), linetype = 1) +
    facet_wrap(~class, ncol = 9, strip.position = "top") +
    theme_bw() +
    ggtitle(paste("Collectors, ", model_name)) +
    theme(strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path("../IMG", model_name, "collector_summary.png"),
         width = 16, height = 12)

  #Arterial
  ggplot( counterfactuals_by_road$arterial %>% 
            filter(!grepl("Error", variable))  %>%
            #have to reshape to seperate out lower, estimate, and upper since I didn't before
            mutate( variable_type = fct_relabel(variable, str_extract, "[A-Za-z0-9\\.]*"),
                    variable_level = fct_relabel(variable, str_extract, paste(levels(d$comfort_rating),collapse="|")),
            ) %>%
            tidyr::spread(variable_type,value) %>%
            group_by(class, person, variable_level) %>% summarize_all(first.nonNA)
  ) +
    geom_ribbon(stat = "identity", aes(x = as.factor(variable_level), ymin = Q2.5, ymax = Q97.5,
                                       group = person, 
                                       fill = person,
                                       color = person), linetype = 3, alpha = .3) +
    geom_line(aes(x = as.factor(variable_level), y = Estimate,
                  group = person,
                  color = person), linetype = 1) +
    facet_wrap(~class, ncol = 9, strip.position = "left") +
    theme_bw() +
    ggtitle(paste("Arterial Streets, ", model_name)) +
    theme(strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(file.path("../IMG", model_name, "arterial_summary.png"),
         width = 20, height = 12)

  } else {
    
    ggplot( tmp.all_counterfactuals %>%
              filter(!grepl("Error", variable), 
              class == first(class), 
              road_type == "collector",
              person == first(person)) %>%
              mutate( variable_type = fct_relabel(variable, str_extract, "[A-Za-z0-9\\.]*"),
                      variable_level = fct_relabel(variable, str_extract, paste(levels(d$comfort_rating),collapse="|"))) %>%
              tidyr::spread(variable_type,value) %>%
              group_by(class, person, variable_level) %>% summarize_all(first.nonNA)
    ) +
      geom_ribbon(stat = "identity", aes(x = as.factor(variable_level), ymin = Q2.5, ymax = Q97.5,
                                         group = person, 
                                         fill = person,
                                         color = person), linetype = 3, alpha = .3, show.legend = F) +
      geom_line(aes(x = as.factor(variable_level), y = Estimate,
                    group = person,
                    color = person),
                    linetype = 1, show.legend = F) +
      theme_bw() + 
      ggtitle(paste("Null Model")) + 
      theme(strip.text = element_text(size = 8),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(file.path("../IMG", model_name, "null.png"),
           width = 16, height = 12)
    
  }
  
})

    #     - Tables (model specific) ----

all_counterfactuals.expected = lapply(models_fitted, function(x) {
  y = filter(x, grepl("Estimate", variable)) %>% 
  droplevels() %>%
  mutate(variable = as.numeric(variable), tmp = variable*value) %>%
  group_by(class, road_environment, person) %>% 
  summarize(expected_value = sum(tmp))
  return(y)
})
  
par(mfrow = c(1,2))

# Models without video random effects

model_fitted_per.table = left_join(all_counterfactuals.expected$int_per, 
          all_counterfactuals.expected$me_per, 
          by = c("class", "road_environment", "person"),
          suffix = c(".int_per", ".me_per"))

plot(x = model_fitted_per.table$expected_value.int_per,
     y = model_fitted_per.table$expected_value.me_per, 
     col = model_fitted_per.table$road_environment,
     xlab = "Interaction model", ylab = "Main effect model",
     main = "ME vs. INT")
legend("topleft", c(levels(model_fitted_per.table$road_environment), "null"), 
       col = 1:7, pch = 16, bty = "n")
abline(a = 0, b =1, lty = 3)
points(all_counterfactuals.expected$null_per$expected_value[1], 
       all_counterfactuals.expected$null_per$expected_value[1], 
       col = 7, pch = 16, cex = 2)

# Models with video random effects

model_fitted_per_vid.table = left_join(all_counterfactuals.expected$int_per_vid, 
                                   all_counterfactuals.expected$me_per_vid, 
                                   by = c("class", "road_environment", "person"),
                                   suffix = c(".int_per", ".me_per"))

plot(x = model_fitted_per_vid.table$expected_value.int_per,
     y = model_fitted_per_vid.table$expected_value.me_per, 
     col = model_fitted_per.table$road_environment,
     xlab = "Interaction model", ylab = "Main effect model",
     main = "ME vs. INT: With video random effects")
legend("topleft", c(levels(model_fitted_per.table$road_environment), "null"),
       col = 1:7, pch = 16, bty = "n")
abline(a = 0, b =1, lty = 3)
points(all_counterfactuals.expected$null_per_vid$expected_value[1], 
       all_counterfactuals.expected$null_per_vid$expected_value[1], 
       col = 7, pch = 16, cex = 2)

#saved in IMG as fitted_me_vs_int_draft.png

# 4. Predictions ----

# Single data point -- stochastic
predict(model1, newdata = model1$data[1,]) 

# Overall
model1.predict = predict(model1) #posterior probs of each class with randomness
model1.predict.ev =  model1.predict %*% c(1:7)
# version of residuals
hist(model1.predict.ev - as.numeric(model1$data$comfort_rating_ordered)) 

# 5. Model comparison table

# model comparison table w/ loo, estimate, sd, n and reff 

#on server
#models_summary = lapply(models, summary)
#models_loo = lapply(models, "[[", "loo")

tmp = readRDS("summary_and_loo.RDS")
models_summary = tmp$models_summary
models_loo = tmp$models_loo
rm(tmp)

models_fixed = lapply(models_summary, "[[", "fixed")
models_fixed = lapply(1:length(models_fixed), function(i) {
  x = data.frame(models_fixed[[i]])
  names(x) = paste0( names(models_fixed)[[i]], names(x))
  x$term = rownames(x)
  return(x)
})

lapply(models_summary, function(x) {x$fixed$Eff.Sample})

models_table = plyr::join_all(models_fixed, type = "left", by = "term")
rownames(models_table) = models_table$term
