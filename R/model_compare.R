# read models with precomputed pointwise loo
library(brms)
models <- lapply(list.files(pattern="_per.RDS|_vid.RDS"), readRDS)
names(models)<-list.files(pattern="_per.RDS|_vid.RDS")

# compare models
compare_loo()

# format or plot below


