nchains = 3
load("to_5i_jul_26.RData")
library(brms)

# Only person random effects -- 
null_per = brm(comfort_rating_ordered ~ (1|person_ID), 
                   data=d.model, 
                   family=cumulative("logit"), iter = 2000,
                   chains = nchains,
                   cores = nchains,
                   control = list(adapt_delta = 0.9),
                   prior = c(set_prior("student_t(3,0,5)", class = "Intercept"),
                             set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(null_per, "null_per.RDS")

# person + video random effects -- 
null_per_vid = brm(comfort_rating_ordered ~ (1|person_ID) + (1|video_name), 
               data=d.model %>% mutate(video_name = d$video_name), 
               family=cumulative("logit"), iter = 2000,
               chains = nchains,
               cores = nchains,
               control = list(adapt_delta = 0.9),
               prior = c(set_prior("student_t(3,0,5)", class = "Intercept"),
                         set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(null_per_vid, "null_per_vid.RDS")