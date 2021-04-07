library(brms)
d.remodel.me <- readRDS("data_for_models.RDS")
nchains = 3


# Main effects + int vary by person + int vary by video + video group vary by video  ---- 
fit<- brm(comfort_rating_ordered ~ . + (1|person_ID) + (1 + VideoGroupWithin |video_name) - person_ID - video_name, 
                 data=d.remodel.me, 
                 family=cumulative("logit"), iter = 2000,
                 chains = nchains,
                 cores = nchains,
                 control = list(adapt_delta = 0.9, max_treedepth = 16),
                 prior = c(set_prior("normal(0,0.5)", class = "b"), 
                           set_prior("normal(0,1.5)", class = "Intercept"),
                           set_prior("student_t(3,0,0.5)", class = "sd"),
                           set_prior("lkj(2)", class = "cor"))
)  
fit <- add_criterion(fit,"loo")
saveRDS(fit, "fit1.RDS")
