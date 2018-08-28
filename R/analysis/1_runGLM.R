if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

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

mod = glm( prelimStateForm,
	family=binomial(link='logit'),
	data=prelimState )
summary(mod)

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

mod = glm( prelimOppForm,
	family=binomial(link='logit'),
	data=prelimOpp )
summary(mod)

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

mod = glm( formalStateForm,
	family=binomial(link='logit'),
	data=formalState )
summary(mod)

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

mod = glm( formalOppForm,
	family=binomial(link='logit'),
	data=formalOpp )
summary(mod)