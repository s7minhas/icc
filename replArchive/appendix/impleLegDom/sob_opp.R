###############################################################
source(paste0(here::here(), '/setup.R'))
loadPkg(c('sbgcop','brms', 'future'))
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
# load democ data for robustness checks
imple = read.csv(paste0(pathData, 'imple_leg_domestic.csv'))

# fix DRC
imple$Country[imple$Country=='DRC'] = 'Congo, Dem. Rep.'

# convert country references to countryname format
imple$Country = cname(imple$Country)

# create country-year version of this imple variable
data$lag1_polity2 = 0

# iterate through countries in imple file, and 
# set lag1_polity2 to 1 based on the Year column
for(ii in 1:nrow(imple)){
	cntry = imple$Country[ii]
	yr = imple$Year[ii]
	data$lag1_polity2[data$cname==cntry & data$year>=yr] = 1
}
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
	file=paste0(pathResults, 'sobOpp_impleLegDom_fin.rda'))
###############################################################