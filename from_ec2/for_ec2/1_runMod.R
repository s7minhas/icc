if(Sys.info()['user'] %in% c('minhas')){
	source('/home/minhas/for_ec2/setup.R') }

loadPkg('xgboost')
loadPkg('bartMachine')
load(paste0(pathData, 'mergedData.rda'))


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

set.seed(6886)
bm_prelimState = bartMachine(
	X = xMat,
	y = y,
	num_trees=50,
	num_burn_in=250,
	num_iterations_after_burn_in=1000,
	alpha=.95, beta=2, k=2, q=.9, nu=3,
	use_missing_data=TRUE
	)
save(bm_prelimState, file=paste0(pathResults,'bm_prelimState.rda'))

## prelim opp
prelimOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
prelimOppForm = formula(
	paste0('prelim_icc_state ~ ', 
		paste(prelimOppVars, collapse = ' + ')
		)
	)
xMat = prelimOpp[,prelimOppVars]
y = prelimOpp$prelim_icc_opp

set.seed(6886)
bm_prelimOpp = bartMachine(
  X = xMat,
  y = y,
  num_trees=50,
  num_burn_in=250,
  num_iterations_after_burn_in=1000,
  alpha=.95, beta=2, k=2, q=.9, nu=3,
  use_missing_data=TRUE
)
save(bm_prelimOpp, file=paste0(pathResults,'bm_prelimOpp.rda'))

## formal state
formalStateVars = c(
	'icc_rat',
	'lag1_poi_osv_state', 
	'lag1_poi_osv_rebel',
	'lag1_poi_pts',
	'lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
formalStateForm = formula(
	paste0('formal_icc_state ~ ', 
		paste(formalStateVars, collapse = ' + ')
		)
	)
xMat = formalState[,formalStateVars]
y = formalState$formal_icc_state

set.seed(6886)
bm_formalState = bartMachine(
  X = xMat,
  y = y,
  num_trees=50,
  num_burn_in=250,
  num_iterations_after_burn_in=1000,
  alpha=.95, beta=2, k=2, q=.9, nu=3,
  use_missing_data=TRUE
)
save(bm_formalState, file=paste0(pathResults,'bm_formalState.rda'))

## formal opp
formalOppVars = c(
	'icc_rat',
	'lag1_poi_osv_state', 
	'lag1_poi_osv_rebel',
	'lag1_poi_pts',
	'lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
formalOppForm = formula(
	paste0('formal_icc_state ~ ', 
		paste(formalOppVars, collapse = ' + ')
		)
	)
xMat = formalOpp[,formalOppVars]
y = formalOpp$formal_icc_opp

set.seed(6886)
bm_formalOpp = bartMachine(
  X = xMat,
  y = y,
  num_trees=50,
  num_burn_in=250,
  num_iterations_after_burn_in=1000,
  alpha=.95, beta=2, k=2, q=.9, nu=3,
  use_missing_data=TRUE
)
save(bm_formalOpp, file=paste0(pathResults,'bm_formalOpp.rda'))