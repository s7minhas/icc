# pth
pth = '~/Research/icc/R/analysis/seqOrd_catEffects/appendix_models/'

# dirs
app_mod_dirs = c(
    'chiefProsec',
    'democ',
    'impleLegDom',
    'noImp',
    'p_US_RUS_CHN',
    'ptsCivilWarOnly', 
    'svac',
    'tradeProp',
    'unsc'
)

# create file paths
mod_files = c(
    paste0(pth, app_mod_dirs, '/sob_state.R'),
    paste0(pth, app_mod_dirs, '/sob_opp.R')
)

# parallelize model runs
library(doParallel)
library(foreach)
cl = makeCluster(length(mod_files)) 
registerDoParallel(cl)
shh = foreach(f = mod_files) %dopar% { source(f) }
stopCluster(cl)