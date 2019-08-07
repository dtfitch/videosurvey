# read models and calculate pointwise loo
library(brms)
models <- lapply(list.files(pattern="_per.RDS|_vid.RDS"), readRDS)
names(models)<-list.files(pattern="_per.RDS|_vid.RDS")
ncores = 15
# this (add_loo?) doesn't work in parallel for some reason. 

# should write a function that reads a brms model, calculates 
# loo using the reloo =T argument, then saves the result into 
# the loo slot in the model object and saves the model again 

models <- lapply(models,function(x) {
  loo_tmp = loo::loo(x, reloo = T, cores = ncores)
  x$loo = loo_tmp
  return(x)
})

saveRDS(models, file = "models_with_loo.RDS")

# Caculate loo for null models on data set from effect models that is slightly reduced

null_loo_reduced_data <- list(
  null_per_loo2 = loo::loo(readRDS("null_per.RDS"), newdata = models$null_per.RDS$data[which(models$null_per.RDS$data$person_ID %in% models$int_per.RDS$data$person_ID),]),
  null_per_vid_loo2 = loo::loo(readRDS("null_per_vid.RDS"), newdata = models$null_per_vid.RDS$data[which(models$null_per_vid.RDS$data$person_ID %in% models$int_per.RDS$data$person_ID),])
)

saveRDS(null_loo_reduced_data, file = "null_loo_reduced_data.RDS")
