if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

loadPkg('randomForest')
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
y = factor(prelimState$prelim_icc_state)

rf = randomForest(x=xMat, y=y, 
	ntree=500
	)

# xgb = xgboost(data = data.matrix(xMat), 
# 	label = y, 
# 	eta = 0.1,
# 	max_depth = 15, 
# 	nround=25, 
# 	subsample = 0.5,
# 	colsample_bytree = 0.5,
# 	seed = 6886,
# 	eval_metric = "auc",
# 	objective = "reg:logistic",
# 	nthread = 3
# )

# xgb.importance(
# 	feature_names=prelimStateVars,
# 	model = xgb
# 	)

set.seed(6886)
bm = bartMachineCV(
	X = xMat,
	y = y,
	# num_trees=50,
	# num_burn_in=250,
	# num_iterations_after_burn_in=1000,
	# alpha=.95, beta=2, k=2, q=.9, nu=3,
	use_missing_data=TRUE
	)

plot_convergence_diagnostics(bm)
plot_y_vs_yhat(bm, credible_intervals = TRUE)
plot_y_vs_yhat(bm, prediction_intervals = TRUE)

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

mod = glm( prelimOppForm,
	family=binomial(link='logit'),
	data=prelimOpp )
summary(mod)

xgb = xgboost(data = data.matrix(xMat), 
	label = y, 
	eta = 0.1,
	max_depth = 15, 
	nround=25, 
	subsample = 0.5,
	colsample_bytree = 0.5,
	seed = 6886,
	eval_metric = "auc",
	objective = "reg:logistic",
	nthread = 3
)

xgb.importance(
	feature_names=prelimOppVars,
	model = xgb
	)

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

mod = glm( formalStateForm,
	family=binomial(link='logit'),
	data=formalState )
summary(mod)

xgb = xgboost(data = data.matrix(xMat), 
	label = y, 
	eta = 0.1,
	max_depth = 15, 
	nround=25, 
	subsample = 0.5,
	colsample_bytree = 0.5,
	seed = 6886,
	eval_metric = "auc",
	objective = "reg:logistic",
	nthread = 3
)

xgb.importance(
	feature_names=formalStateVars,
	model = xgb
	)

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

mod = glm( formalOppForm,
	family=binomial(link='logit'),
	data=formalOpp )
summary(mod)

xgb = xgboost(data = data.matrix(xMat), 
	label = y, 
	eta = 0.1,
	max_depth = 15, 
	nround=25, 
	subsample = 0.5,
	colsample_bytree = 0.5,
	seed = 6886,
	eval_metric = "auc",
	objective = "reg:logistic",
	nthread = 3
)

xgb.importance(
	feature_names=formalOppVars,
	model = xgb
	)