###############################################################
source(paste0(here::here(), '/setup.R'))
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'modData_fin.rda'))

## prelim state
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',
  # p5 vars:
  'lag1_p5_absidealdiffMin'
)

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
###############################################################

###############################################################
# get min using select p5 countries
cntries = c('UNITED.STATES', 'RUSSIAN.FEDERATION', 'CHINA')
pvars = paste0('lag1_', cntries, '_absidealdiff')
data = data %>% 
  drop_na(any_of(pvars)) %>% 
  data.frame(.,stringsAsFactors=FALSE)
data$lag1_p5_absidealdiffMin = apply( 
  data[,pvars], 1, min, na.rm=TRUE )
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
  file=paste0(pathResults, 'sobState_p_US_RUS_CHN_fin.rda'))
###############################################################
