# Load and analyse output of models created in model_comfort_jc.R
# 
# Setup ----
library(brms)
library(ggplot2)
library(bayesplot)
library(rstan)
library(dplyr)

path.to.models = "/Users/jac/Box/Video_CTS/Github/videosurvey/R/"

# LOAD MODELS AS NECESARY ----

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
# int_per_horse = readRDS(file.path(path.to.models, "int_per.RDS"))
# int_per_vid = readRDS(file.path(path.to.models, "int_per.RDS"))
# int_per_vid_horse = readRDS(file.path(path.to.models, "int_per.RDS"))


#         - Evaluate output ----

#model1 = readRDS(file.path(path.to.models, "null_per_vid.RDS"))

model1 = readRDS(file.path(path.to.models, "me_per.RDS"))
#model1 = readRDS(file.path(path.to.models, "me_per_vid.RDS"))
#model1 = readRDS(file.path(path.to.models, "me_per_vid_horse.RDS"))

model1 = readRDS(file.path(path.to.models, "int_per.RDS"))

# Quick summary
summary(model1)

# Diagnostics
check_hmc_diagnostics(model1$fit)

# View coefficients
model1.array = as.array(model1)

# coefficients without video random effects
mcmc_intervals(model1.array, pars = dimnames(model1.array)$parameters[!grepl(dimnames(model1.array)$parameters, pattern = "person_ID|lp__|r_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0))

# only video random effects
mcmc_intervals(model1.array, pars = dimnames(model1.array)$parameters[!grepl(dimnames(model1.array)$parameters, pattern = "person_ID|lp__|b_")], prob_outer = .95) + 
  geom_vline(aes(xintercept = 0))

# Look at videos with large random effects
View(d %>% group_by(video_name) %>% 
       summarize(first(URL), first(veh_volume2_ST), first(veh_volume_ST), first(divided_road_ST),
                 first(bike_operating_space_ST), first(divided_road_ST)))

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