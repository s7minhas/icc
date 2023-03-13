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

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
###############################################################

###############################################################
# load democ data for robustness checks
democ = readRDS(paste0(pathData, 'V-Dem-CY-Full+Others-v13.rds'))

# subset to necessary vars
vars = c( 'country_name', 'year', 'v2x_polyarchy' )
democ = democ[,vars]

# match with existing data and lag
# democ data back a year
democ$year = democ$year + 1
democ$cname = cname(democ$country_name)
democ$cnameYear = with(democ, paste(cname, year, sep='_'))

# merge in polyarchy score in place of polity2
data$lag1_polity2 = democ$v2x_polyarchy[
  match(data$cnameYear,democ$cnameYear)]
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
  file=paste0(pathResults, 'sobState_democ_fin.rda'))
###############################################################