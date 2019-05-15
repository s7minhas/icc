###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('minhas')){
  pathResults=pathData='~/forec2/' }

#
library(brms)
library(foreach)
library(doParallel)
###############################################################

###############################################################
# load data
load(paste0(pathData, 'oppFrame.rda'))
###############################################################

###############################################################
## gen formula
genForms = function(
  vars, 
  dv='icclevel_opp_3', csVarsID=4:length(vars),
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
baseVars = c(
  'icc_rat', 'lag1_civilwar', 
  # 'lag1_polity2', 
  'lag1_gdpCapLog',
  'africa', 'lag1_v2juhcind',
  'lag1_osv_rebel_cumul' )

stratVars = list(
  c('lag1_p5_absidealdiffMin'),
  c('lag1_p5_absidealdiffMax'),
  c('lag1_p5_latAngleMin'),
  c('lag1_p5_defAllyMax'),
  c('lag1_p5_reb_clean'),
  c(
    'lag1_p5_absidealdiffMin',
    'lag1_p5_defAllyMax',
    'lag1_p5_reb_clean'
    )
  )

specs = lapply(stratVars,
  function(x){ c(baseVars, x)})
hierForms = lapply(specs, genForms, pool=FALSE)
###############################################################

###############################################################
# run
cl=makeCluster(24) ; registerDoParallel(cl)
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
      pathResults, '/fromec2/sobOpp_hierSpec',i,'_v3.rda')
  )
}
stopCluster(cl)
###############################################################