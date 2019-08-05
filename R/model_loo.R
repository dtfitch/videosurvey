# read models and calculate pointwise loo
library(brms)
models <- lapply(list.files(pattern="_per.RDS|_vid.RDS"), readRDS)
names(models)<-list.files(pattern="_per.RDS|_vid.RDS")

# this doesn't work in parallel fro some reason. Also, we need
# to do this using the loo() function if we want to re-estimate
# the stan model on the problematic observations. WE probably 
# should write a function that reads a brms model, calculates 
# loo using the reloo =T argument, then saves the result into 
# the loo slot in the model object and saves the model again 
models <- sapply(models,function(x) add_loo(x,cores=15))


