###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## descriptive info for res design portion
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

cntsState=table(data$icclevel_state_3)
totState=sum(cntsState)
propState = cntsState/totState

cntsOpp=table(data$icclevel_opp_3)
totOpp=sum(cntsOpp)
propOpp = cntsOpp/totOpp

cntsState
# round(propState, 2)

cntsOpp
# round(propOpp, 2)

#
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',
  # p5 vars:
  'lag1_p5_absidealdiffMin'
)

ids = c('ccode','cname','year')
ivs = unique(c(sobOppVars, sobStateVars))
dvs = c('icclevel_state_3', 'icclevel_opp_3')
vars = c(ids, ivs, dvs)

# summary(data[,vars])
#
# apply(data[,vars], 2, function(x){
# 	sum(is.na(x))/length(x)
# }) %>% cbind() %>% round(., 2)
#
# dim(data)
data = na.omit(data[,vars])
# dim(data)
# 2906-2336
# 570/2906

cntsState=table(data$icclevel_state_3)
totState=sum(cntsState)
propState = cntsState/totState

cntsOpp=table(data$icclevel_opp_3)
totOpp=sum(cntsOpp)
propOpp = cntsOpp/totOpp

print('listwise deletion')
cntsState
# round(propState, 2)

cntsOpp
# round(propOpp, 2)
(90-77)/90
(77-58)/77
###############################################################