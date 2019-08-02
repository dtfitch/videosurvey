models <- lapply(list.files(pattern="_per.RDS|_vid.RDS"), readRDS)
names(models)<-list.files(pattern="_per.RDS|_vid.RDS")
sapply on this add_loo(models[[5]],cores=6)

loo_compare()
