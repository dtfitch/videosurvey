nchains = 3
load("to_5i_jul_26.RData")
library(brms)

# Main effects + person + video random effects -- 
int_per_vid = brm(comfort_rating_ordered ~ . + (1|person_ID) + (1|video_name) - person_ID - video_name, 
                  data=d.remodel.int, 
                  family=cumulative("logit"), iter = 2000,
                  chains = nchains,
                  cores = nchains,
                  control = list(adapt_delta = 0.9, max_treedepth = 16),
                  prior = c(set_prior("normal(0,5)", class = "b"), 
                            set_prior("student_t(3,0,5)", class = "Intercept"),
                            set_prior("student_t(3,0,5)", class = "sd"))
)  

saveRDS(int_per_vid, "int_per_vid.RDS")
