options("mc.cores" = min(parallel::detectCores(), 4))
load("to_5i_jul_23.RData")
library(brms)

t0 = Sys.time()

# Model with person-level random effects, just top effects discovered before -- 
randord.brms2 = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_name, 
                    data=d.remodel.int, 
                    family=cumulative("logit"), iter = 2000,
                    prior = c(set_prior("normal(0,5)", class = "b"), 
                              set_prior("student_t(3,0,5)", class = "Intercept"),
                              set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms2, "randord_brms2.RDS")

# Model with person-level and video-level random effects, just top effects discovered before -- 
randord.brms2.vid = brm(comfort_rating_ordered ~ (1|person_ID) + (1|video_name) + . - person_ID - video_name, 
                        data=d.remodel.int, 
                        family=cumulative("logit"), iter = 2000,
                        prior = c(set_prior("normal(0,5)", class = "b"), 
                                  set_prior("student_t(3,0,5)", class = "Intercept"),
                                  set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms2.vid, "randord_brms2_vid.RDS")

# Model with person-level random effects, just top effects discovered before + SHRINKAGE prior -- 
randord.brms2.pen = brm(comfort_rating_ordered ~ (1|person_ID) + . - person_ID - video_names, 
                        data=d.remodel.int, 
                        family=cumulative("logit"), iter = 2000,
                        prior = c(set_prior("horseshoe(1)", class = "b"),
                                  set_prior("student_t(3,0,5)", class = "Intercept"),
                                  set_prior("student_t(3,0,5)", class = "sd")))

saveRDS(randord.brms2.pen, "randordbrms2_pen.RDS")

t1 = Sys.time()
print(t1 - t0)