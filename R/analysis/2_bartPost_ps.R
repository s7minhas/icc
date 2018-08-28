if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

loadPkg('bartMachine')
load(paste0(pathData, 'mergedData.rda'))
pathResults = '~/Desktop/'

## prelim state
prelimStateVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
prelimStateForm = formula(
	paste0('prelim_icc_state ~ ', 
		paste(prelimStateVars, collapse = ' + ')
		)
	)
xMat = prelimState[,prelimStateVars]
y = prelimState$prelim_icc_state

#
load(paste0(pathResults, 'bmPrelimState.rda'))