# final_model_predictions.R
# settled on fit2 for publication.
# libraries -----------------------
library(brms)
library(dplyr)
library(ggplot2)
library(ggridges)
# Functions -----------------------
report.brmsfit<-function(x, file=NULL, type="word", digits=3, info=FALSE, 
                         include_ic=FALSE){
  
  sx<-summary(x)
  
  random<-tryCatch(do.call(rbind, sx$random), error=function(e) NA)
  if(!any(is.na(random))) rownames(random)<-paste(rownames(random),rep(names(sx$random), sapply(sx$random, nrow)), sep=" ")
  
  if(include_ic){
    loo<-eval(parse(text="brms::loo(x)"))
    obj<-list(coefficients=setNames(sx$fixed[,1], rownames(sx$fixed)), se=sx$fixed[,2],
              random=random, loo=setNames(c(loo$estimates[1,1], loo$estimates[1,2]), c("ELPD (PSIS-LOO)", "ELPD SE")),
              Eff.Sample_min=sx$fixed[,5], Rhat_max=round(sx$fixed[,6],2))
    output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),obj$Eff.Sample_min,obj$Rhat_max), 
                  if(!any(is.na(random))) {
                    cbind(round(random[,1:2, drop=FALSE], digits), round(random[,5:6, drop=FALSE], digits))
                  },
                  c(round(loo$estimates[1,1], digits), round(loo$estimates[1,2], digits),NA,NA))
    rownames(output)[dim(output)[1]]<-"LOO"
  }else{
    obj<-list(coefficients=setNames(sx$fixed[,1], rownames(sx$fixed)), se=sx$fixed[,2], random=random,
              Rhat=round(sx$fixed[,5],3), Bulk_ESS=sx$fixed[,6], Tail_ESS=sx$fixed[,7])
    output<-rbind(cbind(round(obj$coefficients,digits),round(obj$se,digits),obj$Rhat,obj$Bulk_ESS,obj$Tail_ESS), 
                  if(!any(is.na(random))) {
                    cbind(round(random[,1:2, drop=FALSE], digits), round(random[,5:7, drop=FALSE], digits))
                  })
  }
  
  if(!is.null(file)){
    info <- if(info) deparse(getCall(x)) else NULL
    suppressWarnings(clickR::make_table(output, file, type, info=info))
  }
  obj$output <- data.frame(output, check.names=FALSE, stringsAsFactors=FALSE)
  class(obj) <- "reportmodel"
  invisible(obj)
}
cbind.fill<-function(x,fill=NA){
  x<-lapply(x,as.matrix)
  n<-max(sapply(x,nrow))
  do.call(cbind,lapply(x, function(f)
    rbind(f, matrix(fill,n-nrow(f),ncol(f)))))
}
PCI <- function( samples , prob=0.9 ) {
  #percentile interval from Rethinking
  concat <- function( ... ) {
    paste( ... , collapse="" , sep="" )
  }
  
  x <- sapply( prob , function(p) {
    a <- (1-p)/2
    quantile( samples , probs=c(a,1-a) )
  } )
  # now order inside-out in pairs
  n <- length(prob)
  result <- rep(0,n*2)
  for ( i in 1:n ) {
    low_idx <- n+1-i
    up_idx <- n+i
    # lower
    result[low_idx] <- x[1,i]
    # upper
    result[up_idx] <- x[2,i]
    # add names
    a <- (1-prob[i])/2
    names(result)[low_idx] <- concat(round(a*100,0),"%")
    names(result)[up_idx] <- concat(round((1-a)*100,0),"%")
  }
  return(result)
}

# FUNCTIIONS FOR PLOTTING---------------
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}
# Main Code -------------------------------------------
# read in model
fit1 <- readRDS("fit1.rds")
fit2 <- readRDS("fit2.rds")
parname_dict <- read.csv("parname_dict.csv")

# write summary table
report.brmsfit(fit1, file = "./output/fit_main_effects",
               type = "word", digits = 3, info=FALSE, include_ic=FALSE)
report.brmsfit(fit2, file = "./output/fit_final_model",
               type = "word", digits = 3, info=FALSE, include_ic=FALSE)


# - Make variable name dictionary ------------------

varname_dict = names(fit2$data)
names(varname_dict) = varname_dict

tmp.rename.func <- function(varname_dict, var, newname) {
  x = which(varname_dict == var)
  if (length(x) > 0) {varname_dict[x] =  newname}
  varname_dict
}

varname_dict = tmp.rename.func(varname_dict, "comfort_rating_ordered", "comfort rating")

varname_dict = tmp.rename.func(varname_dict, "female1", "Woman")
varname_dict = tmp.rename.func(varname_dict, "child_u18TRUE", "HH w/ child")
varname_dict = tmp.rename.func(varname_dict, "age", "Age")
varname_dict = tmp.rename.func(varname_dict, "VideoGroupWithin", "'Within' experimental group")

varname_dict = tmp.rename.func(varname_dict, "op_like_biking", "like riding a bike")
varname_dict = tmp.rename.func(varname_dict, "op_need_car", "need car for activities")
varname_dict = tmp.rename.func(varname_dict, "op_feel_safe", "feel safe biking on campus")
varname_dict = tmp.rename.func(varname_dict, "op_like_transit", "like using public transit")
varname_dict = tmp.rename.func(varname_dict, "op_arrive_professional", "job needs professional attire")
varname_dict = tmp.rename.func(varname_dict, "op_travel_stress", "travelling to campus is stressful")
varname_dict = tmp.rename.func(varname_dict, "bike_ability", "confidence level riding a bike")
varname_dict = tmp.rename.func(varname_dict, "comfort_four_no_lane2", "willing to bike on 4-lane road")
varname_dict = tmp.rename.func(varname_dict, "comfort_four_no_lane3", "comfortable biking on 4-lane road")
varname_dict = tmp.rename.func(varname_dict, "usual_mode_4levBike", "usually commute to campus by bike")

varname_dict = tmp.rename.func(varname_dict, "street_parking_ST1", "street parking")
varname_dict = tmp.rename.func(varname_dict, "outside_lane_width_ft_ST", "outside lane width")
varname_dict = tmp.rename.func(varname_dict, "veh_volume2_ST2", "low volume")
varname_dict = tmp.rename.func(varname_dict, "veh_volume2_ST3", "high volume")
varname_dict = tmp.rename.func(varname_dict, "bike_operating_space_ST", "bike operating space")
varname_dict = tmp.rename.func(varname_dict, "bike_lane_SUM_ST1", "bike lane, no buffer")
varname_dict = tmp.rename.func(varname_dict, "bike_lane_SUM_ST2", "bike lane, with buffer")
varname_dict = tmp.rename.func(varname_dict, "speed_prevail_minus_limit_ST", "prevailing minus posted speed")
varname_dict = tmp.rename.func(varname_dict, "speed_limit_mph_ST_3lev.30.40.", "speed limit [30,40)")
varname_dict = tmp.rename.func(varname_dict, "speed_limit_mph_ST_3lev.40.50.", "speed limit [40,50]")
varname_dict = tmp.rename.func(varname_dict, "veh_vol_non0_opspace_0_ST", "bikes share space with cars")

varname_dict = tmp.rename.func(varname_dict, "person_ID", "person ID")
varname_dict = tmp.rename.func(varname_dict, "video_name", "video name")

#more we'll need below
varname_dict2 = setNames(nm = c("ability_comfort", "road_environment", "id", "attitude",
                                "b_Intercept\\[1\\]", "b_Intercept\\[2\\]", "b_Intercept\\[3\\]",
                                "b_Intercept\\[4\\]", "b_Intercept\\[5\\]", "b_Intercept\\[6\\]",
                                "sd_person_ID__Intercept", "sd_video_name__Intercept"),
                         c("biking comfort", "road environment", "id", "transit attitudes",
                           "Intercept 1 ", "Intercept 2", "Intercept 3",
                           "Intercept 4", "Intercept 5", "Intercept 6",
                           "SD person ID Intercept", "SD video name Intercept"))

varname_dict = c(varname_dict, varname_dict2)
rm(varname_dict2)

# reorder varname_dict for plotting
varname_dict <- varname_dict[c(1,2,20,24,3:19,21:23,25:39)]

# - make parameter plots ----------
post1 <-posterior_samples(fit1)[1:35]
post1$model <- "Main Effects"
post2 <-posterior_samples(fit2)[1:42]
post2$model <- "Final"

d.plot <- plyr::rbind.fill(post1,post2)
d.plot <- reshape2::melt(d.plot,"model")
d.plot$variable <- plyr::mapvalues(d.plot$variable,from=parname_dict$pname,to=parname_dict$label)

new.order <- 
  dplyr::inner_join(data.frame(label=levels(d.plot$variable),old_order=1:length(unique(d.plot$variable))),
                  parname_dict,by="label") %>%
  arrange(desc(order))
d.plot$variable <- factor(d.plot$variable,levels=levels(d.plot$variable)[new.order$old_order])

# wrap long labels
levels(d.plot$variable) <- wrap.labels(levels(d.plot$variable),35)

d.plot$model <- factor(d.plot$model)
d.plot$model <- factor(d.plot$model,level=levels(d.plot$model)[c(2,1)])

png(file="output/Figure5a.png",width=6.5,height=9,units="in",res=900,pointsize = 4)
ggplot(d.plot[d.plot$variable %in% levels(d.plot$variable)[42:20],], aes(x = value, y = variable)) + 
  coord_cartesian(xlim = c(-2.5,5.5))+
  geom_density_ridges(scale = 1.2, rel_min_height=0.01) + 
  geom_hline(yintercept=9)+
  geom_hline(yintercept=14)+
  geom_hline(yintercept=18)+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(x = "Parameter (cumulative logit scale)") +
  facet_wrap(~model,nrow=1,labeller = label_wrap_gen(width = 40, multi_line = TRUE))+
  theme(axis.title.y=element_blank(),
        text = element_text(size=12))
dev.off()
png(file="output/Figure5b.png",width=6.5,height=9,units="in",res=900,pointsize = 4)
ggplot(d.plot[d.plot$variable %in% levels(d.plot$variable)[21:1],], aes(x = value, y = variable)) + 
  coord_cartesian(xlim = c(-2.5,5.5))+
  geom_density_ridges(scale = 1.2, rel_min_height=0.01) + 
  geom_hline(yintercept=8)+
  geom_hline(yintercept=19)+
  geom_vline(xintercept=0,linetype="dashed")+
  labs(x = "Parameter (cumulative logit scale)") +
  facet_wrap(~model,nrow=1,labeller = label_wrap_gen(width = 40, multi_line = TRUE))+
  theme(axis.title.y=element_blank(),
        text = element_text(size=12))
dev.off()
# Setup conditions for predictive plots ---------------------------
# summary 

str(fit2$data)
names(fit2$data)

d.model <- readRDS("data_for_models_nonscaled.RDS")

# create by-person data frame for finding quantiles accurately
d.scenario = fit2$data %>% group_by(person_ID) %>% 
  dplyr::select(-c("comfort_rating_ordered","video_name")) %>% summarize_all(first)

#   Building blocks ----
#     - Individual-level ----

#       + attitudes ----

op_levels = data.frame(apply(d.scenario[,-1], 2, quantile, probs = c(.1,.5,.9)))

low_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[1], op_feel_safe = op_levels$op_feel_safe[1], op_like_transit = op_levels$op_like_transit[1])
mid_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[2], op_feel_safe = op_levels$op_feel_safe[2], op_like_transit = op_levels$op_like_transit[2])
high_pos_attitudes = c(op_like_biking = op_levels$op_like_biking[3], op_feel_safe = op_levels$op_feel_safe[3], op_like_transit = op_levels$op_like_transit[3])

low_neg_attitudes = c(op_need_car = op_levels$op_need_car[1], op_arrive_professional = op_levels$op_arrive_professional[1], op_travel_stress = op_levels$op_travel_stress[1])
mid_neg_attitudes = c(op_need_car = op_levels$op_need_car[2], op_arrive_professional = op_levels$op_arrive_professional[2], op_travel_stress = op_levels$op_travel_stress[2])
high_neg_attitudes = c(op_need_car = op_levels$op_need_car[3], op_arrive_professional = op_levels$op_arrive_professional[3], op_travel_stress = op_levels$op_travel_stress[3])

bad_attitudes = c(low_pos_attitudes, high_neg_attitudes)
mid_attitudes = c(mid_pos_attitudes, mid_neg_attitudes)
good_attitudes = c(high_pos_attitudes, low_neg_attitudes)

#       + ability + comfort ----

low_ability_comfort = data.frame(comfort_four_no_lane2=0, comfort_four_no_lane3=0, bike_ability=.5, usual_mode_4levBike = 0) #somewhat confident, low comfort      
mid_ability_comfort = data.frame(comfort_four_no_lane2=1, comfort_four_no_lane3=0, bike_ability=.5, usual_mode_4levBike = 0) #somewaht confident, moderate comfort
high_ability_comfort = data.frame(comfort_four_no_lane2=0, comfort_four_no_lane3=1, bike_ability=1, usual_mode_4levBike = 1) #very confident, high comfort    

#       + demographic ----
agelevels = quantile(d.scenario$age , c(.1,.8,.95))
# On real scale
agelevels*(max(d.model$age,na.rm=T) - min(d.model$age,na.rm=T)) + min(d.model$age,na.rm=T)
# 10% 80% 95% 
# 20  34  57
young_childless_male = data.frame(age = agelevels[1], child_u18TRUE = 0, female1 = 0)
midage_child_female = data.frame(age = agelevels[2], child_u18TRUE = 1, female1 = 1)
old_childless_male = data.frame(age = agelevels[3], child_u18TRUE = 0, female1 = 0)
old_childless_female = data.frame(age = agelevels[3], child_u18TRUE = 0, female1 = 1)

#     - Road environment ----

speed_prevail_levels = quantile(fit2$data$speed_prevail_minus_limit_ST, c(.05,.5,.95))
speed_prevail_levels*(max(d.model$speed_prevail_minus_limit_ST) - min(d.model$speed_prevail_minus_limit_ST)) + min(d.model$speed_prevail_minus_limit_ST)
# 5% 50% 95% 
# -10   0   5  

outside_lane_levels = quantile(fit2$data$outside_lane_width_ft_ST, c(.05,.5,.95))
outside_lane_levels*(max(d.model$outside_lane_width_ft_ST) - min(d.model$outside_lane_width_ft_ST)) + min(d.model$outside_lane_width_ft_ST)
# 5% 50% 95% 
# 9  11  13 

bike_space_levels = quantile(fit2$data$bike_operating_space_ST, c(.05,.5,.95))
bike_space_levels*(max(d.model$bike_operating_space_ST) - min(d.model$bike_operating_space_ST)) + min(d.model$bike_operating_space_ST)
# 5% 50% 95% 
# 0   5  11 


collector_good = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 0,
                            speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 0,
                            bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 1, 
                            speed_prevail_minus_limit_ST = speed_prevail_levels[1],  
                            street_parking_ST1 = 1,
                            outside_lane_width_ft_ST = outside_lane_levels[1],
                            bike_operating_space_ST = bike_space_levels[3], 
                            veh_vol_non0_opspace_0_ST = 0) 

collector_mid = data.frame(veh_volume2_ST2 = 1, veh_volume2_ST3 = 0,
                           speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                           bike_lane_SUM_ST1 = 1, bike_lane_SUM_ST2 = 0,
                           speed_prevail_minus_limit_ST = speed_prevail_levels[2],  
                           street_parking_ST1 = 1,
                           outside_lane_width_ft_ST = outside_lane_levels[2],
                           bike_operating_space_ST = bike_space_levels[2], 
                           veh_vol_non0_opspace_0_ST = 0) 

collector_bad = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                           speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                           bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 0,
                           speed_prevail_minus_limit_ST = speed_prevail_levels[3],  
                           street_parking_ST1 = 1,
                           outside_lane_width_ft_ST = outside_lane_levels[3],
                           bike_operating_space_ST = bike_space_levels[1], 
                           veh_vol_non0_opspace_0_ST = 1) 


arterial_good = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                           speed_limit_mph_ST_3lev.30.40. = 1,  speed_limit_mph_ST_3lev.40.50. = 0,
                           bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 1,
                           speed_prevail_minus_limit_ST = speed_prevail_levels[1],  
                           street_parking_ST1 = 1, # all streets have street parking
                           outside_lane_width_ft_ST = outside_lane_levels[1], #<- outside lane width effect is neg
                           bike_operating_space_ST = bike_space_levels[3], 
                           veh_vol_non0_opspace_0_ST = 0) 

arterial_mid = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                          speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 1,
                          bike_lane_SUM_ST1 = 1, bike_lane_SUM_ST2 = 0, 
                          speed_prevail_minus_limit_ST = speed_prevail_levels[2],  
                          street_parking_ST1 = 1, # all streets have street parking
                          outside_lane_width_ft_ST = outside_lane_levels[2],
                          bike_operating_space_ST = bike_space_levels[2], 
                          veh_vol_non0_opspace_0_ST = 0) 

arterial_bad = data.frame(veh_volume2_ST2 = 0, veh_volume2_ST3 = 1,
                          speed_limit_mph_ST_3lev.30.40. = 0,  speed_limit_mph_ST_3lev.40.50. = 1,
                          bike_lane_SUM_ST1 = 0, bike_lane_SUM_ST2 = 0, 
                          speed_prevail_minus_limit_ST = speed_prevail_levels[3],  
                          street_parking_ST1 = 1, # all streets have street parking
                          outside_lane_width_ft_ST = outside_lane_levels[3],
                          bike_operating_space_ST = bike_space_levels[1], 
                          veh_vol_non0_opspace_0_ST = 1) 

attitudes = data.frame(rbind(bad_attitudes, mid_attitudes, good_attitudes), 
                       id = 1,  attitude = as.factor(c("bad_attitude", "mid_attitude", "good_attitude")))
attitudes$attitude = ordered(attitudes$attitude, levels(attitudes$attitude)[c(1,3,2)])

ability_comfort = data.frame(rbind(low_ability_comfort, mid_ability_comfort, high_ability_comfort), 
                             id = 1,  ability_comfort = as.factor(c("low_comfort", "mid_comfort", "high_comfort")))
ability_comfort$ability_comfort = ordered(ability_comfort$ability_comfort, levels(ability_comfort$ability_comfort)[c(2,3,1)])

road_environments = data.frame(rbind(collector_bad, collector_mid, collector_good,
                                     arterial_bad, arterial_mid, arterial_good),
                               id = 1,  road_environment = c("collector_bad", "collector_mid", "collector_good",
                                                             "arterial_bad", "arterial_mid", "arterial_good"))

road_environments$road_environment = ordered(road_environments$road_environment,
                                             levels = c("arterial_bad", "arterial_mid", "arterial_good", "collector_bad", "collector_mid", "collector_good"))

person = data.frame(rbind(young_childless_male, #midage_child_female, old_childless_female,
                          old_childless_male),
                    id = 1, person =  c("20yr_man", #"midage_child_female", "old_childless_male", 
                                        "57yr_woman"))

all_counterfactuals = plyr::join_all(list(attitudes, ability_comfort, road_environments, person), by='id', type='full' )
all_counterfactuals$rowID = 1:nrow(all_counterfactuals)

building_blocks = list(attitudes = attitudes, ability_comfort = ability_comfort, road_environments = road_environments)

sapply(building_blocks, function(x) {names(x) = varname_dict[names(x)]; x})

#       + Add interactions ----
interaction.terms = trimws(strsplit(as.character(fit2$formula[[1]][3][1]), " \\+ ")[[1]])
interaction.terms = interaction.terms[grepl(":",interaction.terms)]

interactions = data.frame(do.call("cbind", lapply(interaction.terms, function(term) {
  tmp = all_counterfactuals %>% dplyr::select(names(all_counterfactuals)[sapply(names(all_counterfactuals), grepl, x = term)])
  apply(tmp, 1, prod)
})))
names(interactions) = interaction.terms

all_counterfactuals = data.frame(all_counterfactuals, interactions)

# simplified plot for me_per_vid for report ------------
x = rbind(data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, collector_bad)),
          data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, collector_mid)),
          data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, collector_good)),
          data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, arterial_bad)),
          data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, arterial_mid)),
          data.frame(c(bad_attitudes, low_ability_comfort, old_childless_female, arterial_good)))
x$class <- c("bad_attitude.low_comfort.collector_bad",
             "bad_attitude.low_comfort.collector_mid",
             "bad_attitude.low_comfort.collector_good",
             "bad_attitude.low_comfort.arterial_bad",
             "bad_attitude.low_comfort.arterial_mid",
             "bad_attitude.low_comfort.arterial_good")
x$VideoGroupWithin <- rep(0,nrow(x))


# general predictive plots for interactions -----------------------
newdata <- rbind(x[rep(2,9),])
newdata$age <- rep(agelevels,3)
newdata$bike_operating_space_ST <- rep(bike_space_levels,each=3)
newdata2 <- newdata
newdata2$comfort_four_no_lane3 <- 1
newdata<- rbind(newdata,newdata2)

newdata$age_class <- newdata$age*(max(d.model$age,na.rm=T) - min(d.model$age,na.rm=T)) + min(d.model$age,na.rm=T)
newdata$comfort_four_no_lane3_class <- c(rep("NOT comfortable on mixed arterial",9),rep("Comfortable on mixed arterial",9))
newdata$bike_operating_space_ST_class <- newdata$bike_operating_space_ST*(max(d.model$bike_operating_space_ST) - min(d.model$bike_operating_space_ST)) + min(d.model$bike_operating_space_ST)

# predict with data to show interactions
pk <- posterior_epred(fit2,newdata=newdata,allow_new_level=T,sample_new_levels="gaussian")

d.plot <- data.frame(scenario = rep(1:18,each=7),
                     age=rep(paste(newdata$age_class,"yo"),each=7),
                     comfort=rep(newdata$comfort_four_no_lane3_class,each=7),
                     bike_space=rep(newdata$bike_operating_space_ST_class,each=7),
                     class = rep(sort(unique(fit2$data$comfort_rating_ordered)),length(newdata$class)),
                     p.mean = NA,
                     p.lwr = NA,
                     p.upr = NA
)
for(s in 1:length(unique(d.plot$scenario))){
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.mean"] <- apply(pk[,s,],2,mean)
  PI <- apply(pk[,s,],2,PCI,.9)
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.lwr"] <- PI[1,]
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.upr"] <- PI[2,]
}

# age plot
png(file="output/Figure7.png",width=6.5,height=3,units="in",res=900,pointsize = 8)
ggplot(d.plot[d.plot$comfort=="NOT comfortable on mixed arterial",],aes(x=p.mean,y=class))+
  geom_ribbon(aes(xmin=p.lwr,xmax=p.upr,group=scenario,fill=as.factor(bike_space)),alpha=.2)+
  geom_path(aes(group=scenario,color=as.factor(bike_space)))+
  geom_point(aes(color=as.factor(bike_space)))+
  coord_cartesian(xlim=c(0,.8))+
  xlab("Predicted Probability")+
  ylab("")+
  facet_grid(~age)+
  guides(fill=guide_legend(title="Bike operating space (ft)"),
         color=guide_legend(title="Bike operating space (ft)"))+
  theme(legend.position="top")
dev.off()

# comfort plot
png(file="output/Figure8.png",width=6.5,height=3,units="in",res=900,pointsize = 8)
ggplot(d.plot[d.plot$age=="33 yo",],aes(x=p.mean,y=class))+
  geom_ribbon(aes(xmin=p.lwr,xmax=p.upr,group=scenario,fill=as.factor(bike_space)),alpha=.2)+
  geom_path(aes(group=scenario,color=as.factor(bike_space)))+
  geom_point(aes(color=as.factor(bike_space)))+
  coord_cartesian(xlim=c(0,.8))+
  xlab("Predicted Probability")+
  ylab("")+
  facet_grid(~comfort)+
  guides(fill=guide_legend(title="Bike operating space (ft)"),
         color=guide_legend(title="Bike operating space (ft)"))+
  theme(legend.position="top")
dev.off()
# scenario predictive plots ---------------------

# # predict for each sample the response while considering new videos and new people 
# # I.e. include the uncertainty from videos and people in the predictions
# n=10
# pred.cumsum <- array(0, dim = c(3, nrow(x), n))
# for(i in 1:n){
#   pred <- predict(fit2, newdata = x,summary=T,
#                   allow_new_level=T, sample_new_levels="gaussian")
#   tmp <- apply(pred,2,table)
#   if(is.list(tmp)){
#     tmp <- sapply(1:length(tmp),function(x) as.vector(tmp[[x]]))
#     tmp <- cbind.fill(tmp,fill=0)
#   }
#   pred.cumsum[,,i] <- rbind(colSums(tmp[5:7,]),colSums(tmp[6:7,]),tmp[7,])
# }
# pred.cumsum <- apply(pred.cumsum,c(2,3),function(x) x/nrow(pred))
# pred.mean <- apply(pred.cumsum,c(1,2),mean)
# pred.PI <- apply(pred.cumsum,c(1,2),PCI,prob=.95)
# 
# pred.plot <- data.frame(class=rep(x$class,each=3),
#                         road = c(rep("collector",9),rep("arterial",9)),
#                         scenario = rep(rep(c("poor","average","best"),each=3),2),
#                         comfort = rep(c("At least slightly comfortable",
#                                         "At least moderatly comfortable",
#                                         "Very comfortable"),3),
#                         Estimate = as.vector(pred.mean),
#                         Q2.5 = as.vector(pred.PI[1,,]),
#                         Q97.5 = as.vector(pred.PI[2,,]))
# pred.plot$comfort <- factor(pred.plot$comfort, levels = c("At least slightly comfortable",
#                                                           "At least moderatly comfortable",
#                                                           "Very comfortable"))
# pred.plot$scenario <- factor(pred.plot$scenario, levels = c("poor","average","best"))
# 
# #Collector
# ggplot( pred.plot, aes(comfort, Estimate)) +
#   geom_point(size=.8) +
#   #geom_line(aes(group=class)) +
#   geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2)+
#   facet_grid(scenario~road)+#, ncol = 3, strip.position = "top") +
#   theme_bw() +
#   coord_flip()+
#   ylab("Predicted proportion of responses")+
#   #ggtitle(paste("Collectors, ", model_name)) +
#   theme(strip.text = element_text(size = 8),
#         axis.title.y = element_blank())

# Alternative (that I like better) ------------
pk <- posterior_epred(fit2,newdata=x,allow_new_level=T,sample_new_levels="gaussian")

d.plot <- data.frame(scenario=rep(x$class,each=7),
                     road.type=c(rep("Collector",21),rep("Arterial",21)),
                     design=as.factor(rep(rep(c("poor","moderate","good"),each=7),2)),
                     class = rep(sort(unique(fit2$data$comfort_rating_ordered)),length(x$class)),
                     p.mean = NA,
                     p.lwr = NA,
                     p.upr = NA
)
d.plot$design <- factor(d.plot$design,levels=levels(d.plot$design)[c(3,2,1)])
for(s in 1:length(unique(d.plot$scenario))){
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.mean"] <- apply(pk[,s,],2,mean)
  PI <- apply(pk[,s,],2,PCI,.9)
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.lwr"] <- PI[1,]
  d.plot[d.plot$scenario==unique(d.plot$scenario)[s],"p.upr"] <- PI[2,]
}

png(file="output/Figure9.png",width=6.5,height=3,units="in",res=900,pointsize = 8)
ggplot(d.plot,aes(x=p.mean,y=class))+
  geom_ribbon(aes(xmin=p.lwr,xmax=p.upr,group=scenario,fill=as.factor(design)),alpha=.2)+
  geom_path(aes(group=scenario,color=as.factor(design)))+
  geom_point(aes(color=as.factor(design)))+
  coord_cartesian(xlim=c(0,1))+
  xlab("Predicted Probability")+
  ylab("")+
  facet_wrap(~road.type)+
  guides(fill=guide_legend(title="Design Class"),
         color=guide_legend(title="Design Class"))+
  theme(legend.position="top")
dev.off()
