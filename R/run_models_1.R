options("mc.cores" = min(parallel::detectCores(), 4))
load("to_5i_jul_23.RData")
library(brms)

t0 = Sys.time()

# Model with person-level random effects, inclusive coefficient set-- 
randord.brms = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_name, 
                   data=d.remodel, 
                   family=cumulative("logit"), iter = 2000,
                   prior = c(set_prior("normal(0,5)", class = "b"), 
                             set_prior("student_t(3,0,5)", class = "Intercept"),
                             set_prior("student_t(3,0,5)", class = "sd"))
                   )  # ~45 minutes with default priors

saveRDS(randord.brms, "randord_brms.RDS")

# Model with person-level and video-level random effects, inclusive coefficient set-- 
randord.brms.vid = brm(comfort_rating_ordered ~ (1|person_ID) + (1|video_name) + . - person_ID - video_name, 
                       data=d.remodel, 
                       family=cumulative("logit"), iter = 2000,
                       prior = c(set_prior("normal(0,5)", class = "b"), 
                                 set_prior("student_t(3,0,5)", class = "Intercept"),
                                 set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms.vid, "randord_brms_vid.RDS")

# Model with person-level random effects, inclusive coefficient set + SHRINKAGE prior -- 
randord.brms.pen = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_name, 
                       data=d.remodel, 
                       family=cumulative("logit"), iter = 2000,
                       prior = c(set_prior("horseshoe(1)", class = "b"),
                                 set_prior("student_t(3,0,5)", class = "Intercept"),
                                 set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms.pen, "randord_brms_pen.RDS")

t1 = Sys.time()
print(t1 - t0)