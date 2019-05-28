###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('minhas')){
  pathResults=pathData='~/forec2_agg/' }

# install.packages(c('foreach','doParallel','devtools), repos='https://cloud.r-project.org')
# library(devtools)
# install_version("mvtnorm", version = "1.0-8", repos = "http://cran.us.r-project.org")
# install_version("brms", version = "2.8.0", repos = "http://cran.us.r-project.org")

#
library(brms)
library(foreach)
library(doParallel)
###############################################################

###############################################################
# load data
# load(paste0(pathData, 'stateOppFrame.rda'))
frame = read.csv(paste0(pathData, 'stateOppFrame.csv'))
frame = frame[,-1]
###############################################################

###############################################################
## gen formula
genForms = function(
  vars, 
  dv='icclevel_3', csVarsID=5:length(vars),
  pool=FALSE
  ){
  vars[csVarsID] = paste0('cs(',vars[csVarsID],')')  
  form = formula(
    paste0(dv, ' ~ ', 
      paste(vars, collapse = ' + ') ) )
  formHier = formula(
    paste0(dv, ' ~ ', 
      paste(vars, collapse = ' + '), '+(1|id)' ) )
  if(pool){
    return(form)
  } else { return(formHier) } }

# variables
specs = list(
  c(
    'icc_rat', 'lag1_civilwar', 
    'lag1_polity2', 'lag1_gdpCapLog',
    'africa', 'lag1_v2juhcind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    ),
    c(
    'icc_rat', 'lag1_civilwar', 
    'lag1_polity2', 'lag1_gdpCapLog',
    'africa', 'lag1_v2juncind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    ),
  c(
    'icc_rat', 'lag1_civilwar', 
    'africa', 'lag1_v2juhcind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    ),
  c(
    'icc_rat', 'lag1_civilwar', 
    'africa', 'lag1_v2juncind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    ),
  c(
    'icc_rat', 'lag1_civilwar', 
    'lag1_gdpCapLog',
    'africa', 'lag1_v2juhcind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    ),
    c(
    'icc_rat', 'lag1_civilwar', 
    'lag1_gdpCapLog',
    'africa', 'lag1_v2juncind',
    'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul', 
    'lag1_p5_absidealdiffMin'
    )  
  )

hierForms = lapply(specs, genForms, pool=FALSE)
###############################################################

###############################################################
# run
cl=makeCluster(length(hierForms)) ; registerDoParallel(cl)
shh=foreach(i = 1:length(hierForms), 
  .packages=c('brms')) %dopar% {
  mod = brm(
    formula=hierForms[[i]], 
    data=frame,
    family=cratio(link='logit'), 
    cores=4
    )
  save(mod, 
    file=paste0(
      pathResults, 'sobAgg_hierSpec',i,'.rda')
  )
}
stopCluster(cl)
###############################################################