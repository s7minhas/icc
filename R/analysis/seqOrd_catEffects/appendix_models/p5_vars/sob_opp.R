###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_rebel_cumul',
	# p5 vars:
	'lag1_p5_absidealdiffMin' )

# var transformations
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)
data$lag1_osv_rebel_cumul[is.na(data$lag1_osv_rebel_cumul)] = 0
###############################################################

###############################################################
# variations on country ref for ideal pt score
cntries = c(
  'UNITED STATES', 'UNITED KINGDOM', 
  'CHINA', 'RUSSIAN FEDERATION',
  'FRANCE' )
p5_var = paste0('lag1_', cntries, '_absidealdiff')
cshorts = c( 'USA', 'UK', 'CHN', 'RUS', 'FRA' )

# iterate through each changing the absidealdiff
# to the particular country ref and saving the model results
shh = lapply(1:length(cntries), function(ii){

	###############################################################
	# substitute in specific country idealpoint for p5 min score
	data$lag1_p5_absidealdiffMin = data[,p5_var[ii]]
	###############################################################	

	###############################################################
	# org data
	data = data[,c(
		'ccode','cname','year',
		'icclevel_opp_3',
		sobOppVars )]

	# listwise deletion
	frame = na.omit(data)

	# formatting
	frame$icclevel_opp_3 = as.integer(frame$icclevel_opp_3 + 1)
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
	sobOppVars[c(5:8)] = paste0('cs(',sobOppVars[c(5:8)],')')

	# run model
	sobOppForm = formula(
		paste0('icclevel_opp_3 ~ ',
			paste(sobOppVars, collapse = ' + ') ) )
	mod = brm(
		formula=sobOppForm,
		data=frame,
		family=cratio(link='logit'),
		cores=4
		)
	save(
		mod, 
		file=paste0(pathResults, 'sobOpp_p_', cshorts[ii], '_fin.rda'))
	###############################################################
}) # end iteration through p5 countries
###############################################################