# evaluate_interactions.R
library(stringr)

# compare models with person variable interacting with road variables
# to determine which (if any) interaction effects to include.

# (1) start with predictive accuracy (loo) to determine if interactions
# with a single person variable improve predictions
fits <- list.files()[grep("fit",list.files())]
loos <- vector("list",length(fits))
for(f in 1:length(fits)){
  fit <- readRDS(fits[f])
  if(length(fit$criteria)==0){
    # calulate loo and store it
    print(paste("need to calculate loo for this model:", str_remove(fits[f],".RDS")))
    fit <- add_criterion(fit,"loo")
    saveRDS(fit,fits[f])
    loos[[f]] <- fit$criteria
  }else{
    # just store it
    loos[[f]] <- fit$criteria
  }
  
  attr(loos[[f]]$loo,"model_name") <- str_remove(fits[f],".RDS")
}

loo_compare(loos[[1]]$loo,loos[[2]]$loo,loos[[3]]$loo,loos[[4]]$loo,
            loos[[5]]$loo,loos[[6]]$loo,loos[[7]]$loo,loos[[8]]$loo,
            loos[[9]]$loo,loos[[10]]$loo)

#RESULTS:
# only age and comfort_four_no_lane3 interaction models show real prediction improvement
# Move forward with selecting interaction terms from those models

# (2) evaluate "strong" effects (beta/pooled_sd)
fit2_age <- readRDS("fit2_age.RDS")
post <- posterior_samples(fit2_age)
# change data for interaction of interest, test, and repeat
int_effects <- vector("list",length=length(parnames(fit2_age)[grep("age:",parnames(fit2_age))]))
names(int_effects) <- parnames(fit2_age)[grep("age:",parnames(fit2_age))]
for(i in parnames(fit2_age)[grep("age:",parnames(fit2_age))]){
  me <- str_remove(i,"age:")
  int_effects[[i]] <- (post[[me]] + post[[i]]) - post[[me]]
}

sapply(int_effects,mean)
# RESULTS:
# bike operating space, prevail minus limit, 40/50 mph speed limit and opspace0 are all 
# moderated by age much more than the others.

fit2_comfort <- readRDS("fit2_comfort_four_no_lane3.RDS")
post <- posterior_samples(fit2_comfort)
# change data for interaction of interest, test, and repeat
int_effects <- vector("list",length=length(parnames(fit2_comfort)[grep("comfort_four_no_lane3:",parnames(fit2_comfort))]))
names(int_effects) <- parnames(fit2_comfort)[grep("comfort_four_no_lane3:",parnames(fit2_comfort))]
for(i in parnames(fit2_comfort)[grep("comfort_four_no_lane3:",parnames(fit2_comfort))]){
  me <- str_remove(i,"comfort_four_no_lane3:")
  int_effects[[i]] <- (post[[me]] + post[[i]]) - post[[me]]
}

sapply(int_effects,mean)
# RESULTS
# bike operating space,st parking, outside lane width all >|.5|
# (3) propose a new model with specific interactions and compare
# predictive accuracy to the rest