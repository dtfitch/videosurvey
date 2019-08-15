nchains = 3
me_per_vid <- readRDS("me_per_vid.RDS")
library(brms)

# Only person random effects -- 
null_per = brm(comfort_rating_ordered ~ (1|person_ID), 
                   data=me_per_vid$data, 
                   family=cumulative("logit"), iter = 2000,
                   chains = nchains,
                   cores = nchains,
                   control = list(adapt_delta = 0.9),
                   prior = c(set_prior("student_t(3,0,5)", class = "Intercept"),
                             set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(null_per, "null_per_reduced.RDS")

# person + video random effects -- 
library(dplyr)
null_per_vid = brm(comfort_rating_ordered ~ (1|person_ID) + (1|video_name), 
               data=me_per_vid$data, 
               family=cumulative("logit"), iter = 2000,
               chains = nchains,
               cores = nchains,
               control = list(adapt_delta = 0.9),
               prior = c(set_prior("student_t(3,0,5)", class = "Intercept"),
                         set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(null_per_vid, "null_per_vid_reduced.RDS")