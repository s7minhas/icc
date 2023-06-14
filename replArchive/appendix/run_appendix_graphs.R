# pth
pth = '~/Research/icc/R/analysis/'

# get appendix scripts
afiles = list.files(pth)
afiles = afiles[grep('_a_', afiles)]
afiles = paste0(pth, afiles)

# parallelize graph creation
library(doParallel)
library(foreach)
cl = makeCluster(length(afiles)) 
registerDoParallel(cl)
shh = foreach(f = afiles) %dopar% { source(f) }
stopCluster(cl)