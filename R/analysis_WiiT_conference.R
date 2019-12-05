library(brms)
library(brmstools)
library(dplyr)
library(tidyr)
library(ggridges)
library(stringr)
# read data
load("to_5i_jul_26.RData")
d <- d.remodel.me
# Reads in the me_per_vid_varyGender model and creates counterfactual plots
fit <- readRDS("me_per_vid_varyGender.RDS")
# plot forest -------
gg <- forest(fit,grouping="video_name",pars=c("Intercept[1]","Intercept[2]","Intercept[3]",
                                              "Intercept[4]","Intercept[5]","Intercept[6]"))

png(file="Woman_forest.png",width=8.5,height=7.5,units="in",res=1200,pointsize = 12)
gg + 
  xlim(-1,.5)
dev.off()

# read model to use for scenario predicitons -----------
fit <- readRDS("me_per_vid_varyGender.RDS")
fit <- readRDS("me_per_vid.RDS")
fit <- readRDS("me_per.RDS")
d <- fit$data

# data to predict over---------------
# doing 4 cases, good collector, good arterial, bad collector, bad arterial,------------
d.scenario = d %>% group_by(person_ID) %>% select(-"comfort_rating_ordered") %>% summarize_all(first)
op_levels = data.frame(apply(d.scenario[,!(names(d.scenario) %in% c("person_ID","video_name"))],
                             2, quantile, probs = c(.1,.5,.9)))
low_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[1], op_feel_safe = op_levels$op_feel_safe[1], op_like_transit = op_levels$op_like_transit[1])
mid_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[2], op_feel_safe = op_levels$op_feel_safe[2], op_like_transit = op_levels$op_like_transit[2])

mid_neg_attitudes = c(op_need_car = op_levels$op_need_car[2], op_arrive_professional = op_levels$op_arrive_professional[2], op_travel_stress = op_levels$op_travel_stress[2])
high_neg_attitudes = c(op_need_car = op_levels$op_need_car[3], op_arrive_professional = op_levels$op_arrive_professional[3], op_travel_stress = op_levels$op_travel_stress[3])

bad_attitudes = c(low_pos_attitudes, high_neg_attitudes)
mid_attitudes = c(mid_pos_attitudes, mid_neg_attitudes)

cases=4
newdata <- data.frame(female1 = rep(1,cases),
                      child_u18TRUE = rep(1,cases),
                      age_impute = rep(.258,cases), #.258 = 34 yrs, .645 = 57 yrs
                      
                      # low comfort but good ability to ride a bike
                      comfort_four_no_lane2 = rep(0,cases), 
                      comfort_four_no_lane3 = rep(0,cases), 
                      bike_ability = rep(.5,cases), 
                      usual_mode_4levBike = rep(0,cases), 
                      
                      # bad attitudes
                      # op_like_biking = rep(0.4,cases),
                      # op_feel_safe = rep(0.4,cases),
                      # op_like_transit = rep(0.2,cases),
                      # op_need_car = rep(1,cases),
                      # op_arrive_professional = rep(1,cases),
                      # op_travel_stress = rep(.8,cases),
                      
                      # mid attitudes
                      op_like_biking = rep(.8,cases),
                      op_feel_safe = rep(.8,cases),
                      op_like_transit = rep(.6,cases),
                      op_need_car = rep(.8,cases),
                      op_arrive_professional = rep(.6,cases),
                      op_travel_stress = rep(.6,cases),
                      
                      speed_prevail_minus_limit_ST  = c(.1,.85,.1,.85),
                      outside_lane_width_ft_ST = c(.167,.833,.167,.833),
                      bike_operating_space_ST = c(.846,0,.846,0),
                      veh_volume2_ST2 = rep(0,cases), 
                      veh_volume2_ST3 = c(0,1,1,1),
                      speed_limit_mph_ST_3lev.30.40. = c(0,1,1,0), 
                      speed_limit_mph_ST_3lev.40.50. = c(0,0,0,1),
                      bike_lane_SUM_ST1 = rep(0,cases),
                      bike_lane_SUM_ST2 = c(0,1,0,1),
                      street_parking_ST1 = rep(1,cases),
                      veh_vol_non0_opspace_0_ST = c(0,1,0,1),
                      scenario = c("Good Collector", "Bad Collector", "Good Arterial", "Bad Arterial"))

# for(i in 1:length(unique(d$video_name))){
#   newdata$video_name <- rep(unique(d$video_name)[i],cases)
#   if(i==1) newdata.video <- newdata
#   else newdata.video <- rbind(newdata.video, newdata)
# }

# predict
pred <- predict(fit, newdata=newdata, re_formula= NA,summary=F)

cat.names <- attributes(pred)$levels
pred <- as.data.frame(pred)
# names(pred) <- paste0(newdata.video$scenario,"_", newdata.video$video_name)
names(pred) <- newdata$scenario
#pred$sim <- 1:nrow(pred)
pred <- pred %>% gather()
pred$response <- recode(pred$value, `1`="Very uncomfortable",
                              `2`="Moderately uncomfortable",             
                              `3`="Slightly uncomfortable)",               
                              `4`="Neither comfortable nor uncomfortable",
                              `5`="Slightly comfortable",                 
                              `6`="Moderately comfortable",               
                              `7`="Very comfortable")

png(file="./IMG/WiiT_conference/Best_For_Women.png",width=8.5,height=7.5,units="in",res=1200,pointsize = 16)
ggplot(pred, aes(y=key,x=value))+
  geom_density_ridges(stat="binline", bins=7,scale=.95,rel_min_height=.01)+
  scale_x_discrete(limits=1:7,labels = c("Very uncomfortable",
                                                     "Moderately uncomfortable",             
                                                     "Slightly uncomfortable",               
                                                     "Neither comfortable nor uncomfortable",
                                                     "Slightly comfortable",                 
                                                     "Moderately comfortable",               
                                                     "Very comfortable"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        text = element_text(size=16))
dev.off()
  
# doing 2 cases man and woman with interaction model-----------------
fit <- readRDS("int_per_byGender.RDS")

d.scenario = d %>% group_by(person_ID) %>% select(-"comfort_rating_ordered") %>% summarize_all(first)
op_levels = data.frame(apply(d.scenario[,!(names(d.scenario) %in% c("person_ID","video_name"))],
                             2, quantile, probs = c(.1,.5,.9)))
mid_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[2], op_feel_safe = op_levels$op_feel_safe[2], op_like_transit = op_levels$op_like_transit[2])
mid_neg_attitudes = c(op_need_car = op_levels$op_need_car[2], op_arrive_professional = op_levels$op_arrive_professional[2], op_travel_stress = op_levels$op_travel_stress[2])
mid_attitudes = c(mid_pos_attitudes, mid_neg_attitudes)

cases=2
newdata <- data.frame(female1 = c(0,1),
                      child_u18TRUE = rep(0,cases),
                      age_impute = rep(.258,cases), #.258 = 34 yrs, .645 = 57 yrs
                      
                      # low comfort but good ability to ride a bike
                      comfort_four_no_lane2 = rep(0,cases), 
                      comfort_four_no_lane3 = c(0,0), 
                      bike_ability = rep(.5,cases), 
                      usual_mode_4levBike = rep(0,cases), 
                      
                      # bad attitudes
                      # op_like_biking = rep(0.4,cases),
                      # op_feel_safe = rep(0.4,cases),
                      # op_like_transit = rep(0.2,cases),
                      # op_need_car = rep(1,cases),
                      # op_arrive_professional = rep(1,cases),
                      # op_travel_stress = rep(.8,cases),
                      
                      # mid attitudes
                      op_like_biking = rep(.8,cases),
                      op_feel_safe = rep(.8,cases),
                      op_like_transit = rep(.6,cases),
                      op_need_car = rep(.8,cases),
                      op_arrive_professional = rep(.6,cases),
                      op_travel_stress = rep(.6,cases),
                      
                      speed_prevail_minus_limit_ST = rep(.5,cases),
                      outside_lane_width_ft_ST = rep(.5,cases),
                      bike_operating_space_ST = rep(.5,cases),
                      veh_volume2_ST2 = rep(0,cases), 
                      veh_volume2_ST3 = rep(0,cases),
                      speed_limit_mph_ST_3lev.30.40. = rep(0,cases),
                      speed_limit_mph_ST_3lev.40.50. = rep(0,cases),
                      bike_lane_SUM_ST1 = rep(0,cases),
                      bike_lane_SUM_ST2 = rep(0,cases),
                      street_parking_ST1 = rep(1,cases),
                      veh_vol_non0_opspace_0_ST = rep(0,cases),
                      scenario = c("Man","Woman"))

# predict
pred <- predict(fit, newdata=newdata, re_formula= NA,summary=T)
library(reshape2)
pred <- melt(pred)
pred$gender <- rep(c("Man","Woman"),7)

png(file="./IMG/WiiT_conference/Woman_vs_Man.png",width=8.5,height=7.5,units="in",res=1200,pointsize = 16)
ggplot(pred, aes(y=key,x=value))+
  geom_density_ridges(stat="binline", bins=7,scale=.95,rel_min_height=.01)+
  scale_x_discrete(limits=1:7,labels = c("Very uncomfortable",
                                         "Moderately uncomfortable",             
                                         "Slightly uncomfortable",               
                                         "Neither comfortable nor uncomfortable",
                                         "Slightly comfortable",                 
                                         "Moderately comfortable",               
                                         "Very comfortable"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        text = element_text(size=16))
dev.off()

ggplot(pred,aes(Var2,value,fill=gender))+
  geom_histogram(stat="identity",position="dodge")+
  coord_flip()+
  ylab("Predicted Probability")+
  xlab("")+
  scale_fill_discrete(name = "", labels = c("Woman",
                                            "Man"))+
  theme(legend.position = "top")

# Simulation of new Women ----------------

# how many simulated actors would you like?
n_sim <- 50

f <-
  fitted(fit,
         newdata = newdata,
         probs = c(.1, .9),
         allow_new_levels = T,
         sample_new_levels = "gaussian",
         summary = F,
         nsamples = n_sim) %>%
  as_tibble() %>%
  mutate(iter = 1:n_sim) %>%
  gather(key, value, -iter) %>%
  bind_cols(newdata %>%
              transmute(condition = str_c(prosoc_left, "/", condition) %>%
                          factor(., levels = c("0/0", "1/0", "0/1", "1/1"))) %>%
              expand(condition, iter = 1:n_sim))


p6 <-
  f %>%
  
  ggplot(aes(x = condition, y = value, group = iter)) +
  geom_line(alpha = 1/2, color = "blue") +
  ggtitle("50 simulated actors") +
  coord_cartesian(ylim = 0:1) +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 14, hjust = .5))

p6

# Comparison of Men and Women with non video model an dinteractions for road effects -----
fit <- readRDS("int_per_byGender.RDS")

post <- posterior_samples(fit,pars = names(fit$fit)[c(17:21,23:24,26:28,30:35,37:42)])
post <- post %>% 
        gather() 
post$gender <- ifelse(grepl("b_female",post$key),"Woman","Man")
post$key <- ifelse(post$gender=="Man",str_remove(post$key, "b_"),str_remove(post$key, "b_female1:"))
post[post$gender=="Woman","value"] <-post[post$gender=="Woman","value"] + post[post$gender=="Man","value"]

post$key <- ifelse(post$key=="street_parking_ST1", "street parking",post$key)
post$key <- ifelse(post$key=="outside_lane_width_ft_ST", "outside lane width",post$key)
post$key <- ifelse(post$key=="veh_volume2_ST2", "Med volume",post$key)
post$key <- ifelse(post$key=="veh_volume2_ST3", "high volume",post$key)
post$key <- ifelse(post$key=="bike_operating_space_ST", "bike operating space",post$key)
post$key <- ifelse(post$key=="bike_lane_SUM_ST1", "bike lane",post$key)
post$key <- ifelse(post$key=="bike_lane_SUM_ST2", "bike lane with buffer",post$key)
post$key <- ifelse(post$key=="speed_prevail_minus_limit_ST", "prevailing minus posted speed",post$key)
post$key <- ifelse(post$key=="speed_limit_mph_ST_3lev.30.40.", "speed limit [30,40)",post$key)
post$key <- ifelse(post$key=="speed_limit_mph_ST_3lev.40.50.", "speed limit [40,50]",post$key)
post$key <- ifelse(post$key=="veh_vol_non0_opspace_0_ST", "bikes share space with cars",post$key)


png(file="./IMG/WiiT_conference/Woman_vs_Man_params.png",width=10.5,height=7.5,units="in",res=1200,pointsize = 16)
ggplot(post, aes(x=value,y=key,fill=gender))+
  geom_density_ridges(alpha=.6,rel_min_height=.01)+
  coord_cartesian(xlim=c(-4,4))+
  xlab("Parameter (Log-cumulative odds)")+
  theme(axis.title.y = element_blank(),
        text = element_text(size=16))
dev.off()
