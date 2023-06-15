# pth
pth = paste0(here::here(), '/appendix/')

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

# run descriptive analysis for A.15 of appendix
source(
    paste0(here::here(), 
    '/descriptive/fig_a16_descStats.R')
    )