###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop', 'brms', 'future'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',	
  # p5 vars: 
  'lag1_p5_absidealdiffMin' )
###############################################################  

###############################################################
# use counter for last time since civilian targeting
data = data[order(data$cname, data$year),]
data$lag1_osv_time = 0
cntries = unique(data$cname)

# iterate through and get counts
data = lapply(cntries, function(cntry){

  # pull out slice for specific country
  slice = data[data$cname == cntry,]

  # start by setting counter to zero, 
  # when there is a peace year add 1
  # if back to conflict reset to zero
  counter = 0
  for(ii in 1:nrow(slice)){
    if(slice[ii,'lag1_osv_state_cumul'] > 0 ){
      counter = 0
    } else {
      counter = counter + 1 }
  slice$lag1_osv_time[ii] = counter }

  #
  return(slice) })

# recombine
data = do.call('rbind', data)

# replace existing vars so we can keep same code
data$lag1_osv_state_cumul = data$lag1_osv_time
###############################################################

###############################################################
# org data
data = data[,c(
  'ccode','cname','year',
  'icclevel_state_3',
  sobStateVars )]

# listwise deletion
frame = na.omit(data)

# formatting
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
frame$ccode = as.integer(frame$ccode)
###############################################################

###############################################################
# create p5 variable (2, 365, 220, 710, 200)
frame$p5 = ifelse(
  frame$ccode %in% c(2, 365, 220, 710, 200), 0, 1 )

# modify p5 var to be zero if p5 country
frame$lag1_p5_absidealdiffMin = frame$p5*frame$lag1_p5_absidealdiffMin
###############################################################

###############################################################
# category specific effects
sobStateVars[c(5:8)] = paste0('cs(',sobStateVars[c(5:8)],')')

# run model
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ',
		paste(sobStateVars, collapse = ' + ') ) )
mod = brm(
	formula=sobStateForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
	)
save(
  mod, 
  file=paste0(pathResults, 'sobState_intJust_fin.rda'))
###############################################################