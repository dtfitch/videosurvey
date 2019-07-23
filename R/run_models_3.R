options("mc.cores" = min(parallel::detectCores(), 4))
load("to_5i_jul_23.RData")
library(brms)

t0 = Sys.time()

# Model with person-level random effects, exclusive coefficient set + interactions-- 
randord.brms3 = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_name, 
                    data=d.remodel.int2, 
                    family=cumulative("logit"), iter = 2000,
                    prior = c(set_prior("normal(0,5)", class = "b"), 
                              set_prior("student_t(3,0,5)", class = "Intercept"),
                              set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms3, "randord_brms3.RDS")

# Model with person-level and video-level random effects, exclusive coefficient set + interactions- -- 
randord.brms3.vid = brm(comfort_rating_ordered ~ (1|person_ID) + (1|video_name) + . - person_ID - video_name, 
                        data=d.remodel.int2, 
                        family=cumulative("logit"), iter = 2000,
                        prior = c(set_prior("normal(0,5)", class = "b"), 
                                  set_prior("student_t(3,0,5)", class = "Intercept"),
                                  set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms3.vid, "randord_brms3_vid.RDS")

# # Model with person-level random effects, xclusive coefficient set + interactions-+ SHRINKAGE prior -- 
randord.brms3.pen = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_names, 
                        data=d.remodel.int2, 
                        family=cumulative("logit"), iter = 2000,
                        prior = c(set_prior("horseshoe(1)", class = "b"),
                                  set_prior("student_t(3,0,5)", class = "Intercept"),
                                  set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms3.pen, "randord_brms3_pen.RDS")

t1 = Sys.time()
print(t1 - t0)