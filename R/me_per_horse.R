nchains = 3
load("to_5i_jul_26.RData")
library(brms)

# Main effects + person random effects + horshoe prior on beta  -- 
me_per_horse = brm(comfort_rating_ordered ~ . + (1|person_ID) - person_ID - video_name, 
                   data=d.remodel.me, 
                   family=cumulative("logit"), iter = 2000,
                   chains = nchains,
                   cores = nchains,
                   control = list(adapt_delta = 0.9),
                   prior = c(set_prior("horseshoe(1)", class = "b"), 
                             set_prior("student_t(3,0,5)", class = "Intercept"),
                             set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(me_per_horse, "me_per_horse.RDS")