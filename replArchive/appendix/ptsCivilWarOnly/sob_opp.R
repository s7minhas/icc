###############################################################
source(paste0(here::here(), '/setup.R'))
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'modData_fin.rda'))

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
# only incl states with civ war or pts>=3 since 2002
load(paste0(pathData, 'subset_ptsCivWar_cntries.rda'))

## keep only states in countries vector
frame = frame[frame$ccode %in% cntries$ccode,]

# remove civil war covariate
sobOppVars = sobOppVars[-which(sobOppVars %in% c('lag1_civilwar'))]
###############################################################

###############################################################
# category specific effects
sobOppVars[c(4:7)] = paste0('cs(',sobOppVars[c(4:7)],')')

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
save(mod, 
	file=paste0(pathResults, 
		'sobOpp_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda'))
###############################################################