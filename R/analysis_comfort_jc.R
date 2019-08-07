# Load and analyse output of models created in model_comfort_jc.R
# 
# Notes:
# Didn't analyze models with horshoe prior
# Have main and int, with and without video re, two nulls <- 6 total models
#
#
# TO DO: 
# run scenarios on sheets - try for me and int (since we have it) but we'll probably only display me model.
#
# ~5 ft for bike lane, ~7ft for protected bike land
#individual cases:
# means for attitudes
#
# veh_vol_non0_opspace_0_ST
# bike_operating_space_ST  
# street_parking_ST1
# bike_lane_SUM_ST1 
# bike_lane_SUM_ST2  
# veh_volume2_ST3
# bike_lane_SUM_ST2                   
# speed_limit_mph_ST_3lev.40.50.
# speed_limit_mph_ST_3lev.30.40.  
#
# pp_check, ppc_bars , e..g. pp_check(null2.loo, type = "bars", nsamples = "100")
# brms tools forest for random effects
# brmstools for coefficients?


# Setup ----
library(brms)
library(ggplot2)
library(bayesplot)
library(rstan)
library(dplyr)
library(cowplot)

path.to.models = "/Users/jac/Box/Video_CTS/Github/videosurvey/R/"


# model loads ----

models = readRDS("models_with_loo.RDS")
names(models)

int_per.array = as.array(models$int_per.RDS)
int_per_vid.array = as.array(models$int_per_vid.RDS)
me_per.array = as.array(models$me_per.RDS)
me_per_vid.array = as.array(models$me_per_vid.RDS)

null_per.array = as.array(models$null_per.RDS)

#   (DEPRECATED) individual model loads  ----

# Null models
# null_per = readRDS(file.path(path.to.models, "null_per.RDS"))
# null_per_vid = readRDS(file.path(path.to.models, "null_per_vid.RDS"))

# Main effects 
# me_per = readRDS(file.path(path.to.models, "me_per.RDS"))
# me_per_horse = readRDS(file.path(path.to.models, "me_per_horse.RDS"))
# me_per_vid = readRDS(file.path(path.to.models, "me_per.RDS"))
# me_per_vid_horse = readRDS(file.path(path.to.models, "me_perv.RDS"))
# 
# # Main effects + interaction effects
# int_per = readRDS(file.path(path.to.models, "int_per.RDS"))
# int_per_horse = readRDS(file.path(path.to.models, "int_per_horse.RDS"))
# int_per_vid = readRDS(file.path(path.to.models, "int_per_vid.RDS"))
# int_per_vid_horse = readRDS(file.path(path.to.models, "int_pe_vid_horse.RDS"))

# Compare loo ----

loo_compare("me_per.RDS" = models$me_per.RDS$loo, 
            "me_per_vid.RDS" = models$me_per_vid.RDS$loo, 
            "int_per.RDS" =models$int_per.RDS$loo, 
            "int_per_vid.RDS" = models$int_per_vid.RDS$loo, 
            null_per_loo2,
            null_per_vid_loo2)

#         - Evaluate output ----


null_per_vid.array = as.array(models$null_per_vid.RDS)

# [1] "int_per.RDS"      "int_per_vid.RDS"  "me_per.RDS"       "me_per_vid.RDS"   "null_per.RDS"     "null_per_vid.RDS"

# Quick summary
summary(models$int_per.RDS)

# Diagnostics
check_hmc_diagnostics(models$null_per$fit)
sapply(models, loo)

# View coefficients

# coefficients without video random effects
mcmc_intervals(int_per_vid.array, pars = dimnames(model1.array)$parameters[!grepl(dimnames(model1.array)$parameters, pattern = "person_ID|lp__|r_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0))

# only video random effects
mcmc_intervals(int_per_vid.array, pars = dimnames(model1.array)$parameters[!grepl(dimnames(model1.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0))

# Look at videos with large random effects
View(d %>% group_by(video_name) %>% 
       summarize(first(URL), first(veh_volume2_ST),first(bike_lane_SUM_ST),
                 first(bike_operating_space_ST), first(speed_limit_mph_ST), first(bike_speed_mph_ST)))

# Compare two model coefficients

model2.array = as.array(models$int_per_vid.RDS)

# coefficients without video random effects
plot_grid(
mcmc_intervals(me_per.array, pars = dimnames(me_per.array)$parameters[!grepl(dimnames(me_per.array)$parameters, pattern = "person_ID|lp__|r_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0)),
mcmc_intervals(int_per.array, pars = dimnames(int_per.array)$parameters[!grepl(dimnames(int_per.array)$parameters, pattern = "person_ID|lp__|r_|hs_c2")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0)), 
mcmc_intervals(me_per_vid.array, pars = dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters, pattern = "person_ID|lp__|r_|hs_c2")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0)),
mcmc_intervals(int_per_vid.array, pars = dimnames(int_per_vid.array)$parameters[!grepl(dimnames(int_per_vid.array)$parameters, pattern = "person_ID|lp__|r_|hs_c2")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0)), nrow = 2
)


# just video random effects
plot_grid(
mcmc_intervals(me_per_vid.array, pars = dimnames(me_per_vid.array)$parameters[!grepl(dimnames(me_per_vid.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0)),
mcmc_intervals(int_per_vid.array, pars = dimnames(int_per_vid.array)$parameters[!grepl(dimnames(int_per_vid.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0))
)


#         + Predictions ----

# Single data point -- stochastic
predict(model1, newdata = model1$data[1,]) 

# Overall
model1.predict = predict(model1) #posterior probs of each class with randomness
model1.predict.ev =  model1.predict %*% c(1:7)
# version of residuals
hist(model1.predict.ev - as.numeric(model1$data$comfort_rating_ordered)) 

#         + Random Effects ----

# Many large person-level random effects -- do we want a more restrictive prior?
pi.samples = posterior_samples(model1, pars = "person_ID")
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
pi.samples.melt = melt(data.frame(pi.samples[,which(abs(pi.means) > 3 )]))
pi.samples.melt$variable = factor(pi.samples.melt$variable,
                                  levels = unique(pi.samples.melt$variable)[order(pi.means[which(abs(pi.means) > 3 )])])
ggplot(data = pi.samples.melt) + 
  geom_density_ridges(aes(x = value, group = variable, y = variable))

#         + Compare predictions from ordinal model with and without random effects ----

# Make data frame of response and model predictions (as expected values)
ord.prelim.predict = data.frame(predict.ord = apply(predict(glmcr.prelim, type = "probs")$probs, 1:2, mean) %*%  c(1:7), 
                                #predict.ord.mode = as.numeric(predict(glmcr.prelim, type = "class")),
                                person_ID = d.remodel.int$person_ID,
                                video_name = d.remodel.int$video_name)

ord.predict.compare = left_join(ord.prelim.predict, data.frame(randord.brms.predict.ev,
                                                               person_ID = randord.brms$data$person_ID, 
                                                               # to fix
                                                               video_name = d.remodel[apply(d.remodel, 1, function(x) {sum(is.na(x)) == 0}), "video_name"]), 
                                by = c("person_ID", "video_name"))

ord.predict.compare$response = as.numeric(ord.prelim$model$comfort_rating_ordered)
ord.predict.compare = ord.predict.compare %>% arrange(response)
plot(ord.predict.compare$predict.ord, ord.predict.compare$randord.brms.predict.ev)

#         + Compare by plotting ----

#           - in base ----
par(mfrow = c(1,3))
plot(ord.predict.compare$predict.ord.mode, col = "red", type = "p", ylim = c(1,7), 
     main = "no random effects - modal response")
points(ord.predict.compare$response, col = "black", type = "l")
plot(ord.predict.compare$predict.ord, col = "red", type = "p", ylim = c(1,7), main = "no random effects - expected")
points(ord.predict.compare$response, col = "black", type = "l")
plot(ord.predict.compare$predict.brms, col = "green", type = "p",ylim = c(1,7), main = "random effects")
points(ord.predict.compare$response, col = "black", type = "l")

#residuals + noise
plot(rnorm(nrow(ord.predict.compare),0,.5) + (ord.predict.compare$predict.brms - ord.predict.compare$response)[order(ord.predict.compare$predict.brms)], pch = ".")

#           - in ggplot ----
plot_grid(
  ggplot(ord.predict.compare) + 
    geom_hex(aes(x = 1:nrow(ord.predict.compare), y = predict.ord), bins = 10) +
    geom_line(aes(x = 1:nrow(ord.predict.compare), y = response), color = "red", size = 4),
  ggplot(ord.predict.compare) + 
    geom_hex(aes(x = 1:nrow(ord.predict.compare), y = predict.brms), bins = 10) +
    geom_line(aes(x = 1:nrow(ord.predict.compare), y = response), color = "red", size = 4)
)
ggsave("IMG/predictions_no_rand_vs_rand.png")

#           - by histogram ----
par(mfrow = c(1,1))
hist(ord.predict.compare$predict.ord - ord.predict.compare$response, col = "gray", ylim = c(0,5000), breaks = 20,
     main = "Comparison of residuals of model with (green) and without (gray) random effects")
hist(ord.predict.compare$predict.brms - ord.predict.compare$response, add = T, col = "green", density = 30, breaks = 20)

#           - by quantiles ----
round(quantile(ord.predict.compare$predict.ord- ord.predict.compare$response, seq(0.05,.95,.05)),2)
round(quantile(ord.predict.compare$predict.brms - ord.predict.compare$response, seq(0.05,.95,.05)),2)