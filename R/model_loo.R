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