# slice_analysis.R

# conduct some visual analysis of predictive ability of models on 
# subsets of data (slices) to determine if subsets should be excluded, or
# if the models could improve with specification changes. Use models
# from report in ~/Video_CTS/Github/videosurvey/R

# Start with full model and compare in-sample predictions for "slices"
# through visual posterior predictive checks.
fit <- readRDS("me_per_vid.RDS")
d <- fit$data

# Predictions by extreme responses ----------------
person_response <- as.data.frame(table(d$person_ID,d$comfort_rating_ordered))
fit$data$person_novariation <- ifelse(d$person_ID %in% person_response[person_response$Freq==5,"Var1"],1,0)

pp_check(fit,type="bars_grouped",group="person_novariation",nsamples=300)

# Notes:
# results clearly show that people who always select neutral are poorly predicted.
# This in turn biases predictions of neutral responses for people who select neutral
# for less than all videos. People who don't vary their response but who select a
# class other than neutral, don't seem to be problematic. In fact, people who always
# select Very comfortable" are predicted quite well. I think this suggests that the
# only people we have to worry about are the "always neutral" respondents
# (n = 97 people or 485 responses or 3.2% of the data)
length(unique(fit$data[fit$data$person_novariation==1 & 
           fit$data$comfort_rating_ordered=="Neither comfortable nor uncomfortable","person_ID"]))

# Predictions by person IVs as "slices"-------------
slices <- names(d[2:11])
for(s in slices){
  print(
  pp_check(fit,type="bars_grouped",group=s,nsamples=300)+ggtitle(s)
  )
  readline(prompt="Press [enter] to continue")
}

# Notes: nothing striking here. Model predictions seem to be close to data for
# all IV subsets. Great!

# TO DO: repeat analysis after other model updates, make final decision on whether
# to remove or retain "always neutral" respondents.