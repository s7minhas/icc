###############################################################
source('~/Research/icc/R/setup.R')
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
# add in alternative p5 data
# comes from github.com/s7minhas/plutonium
load(paste0(pathData, 'statePref_from_plutonium.rda'))

# lag dyadData back one year
dyadData$year = dyadData$year + 1

# subset to sample frame
dyadData = dyadData[dyadData$year %in% 2002:2016,]

# subset to relevant vars
ids = c('cname1', 'cname2', 'year')
vars = 'trade'
dyadData = dyadData[,c(ids, vars)]

# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
dyadData = dyadData[which(dyadData$cname2 %in% toKeep),]

# spread data and org pref measures into sep lists
dyadData = dyadData %>% 
	gather(variable, value, -(cname1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)

# org for calculation
pData = lapply(toKeep, function(v){
	slice = dyadData[which(dyadData$cname1 == v),]
	slice[,which(grepl(v, names(dyadData)))] = NA
	return(slice) })
pData[[length(pData) + 1 ]] = dyadData[which(!dyadData$cname1 %in% toKeep),]

# loop over each using p5Vars function
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_', vars), vars) })

# reorg
dyadData = do.call('rbind', pData)

# merge into data
ddid = paste0(dyadData$cname1, '_', dyadData$year)
did = paste0(data$cname, '_', data$year)
toMerge = names(dyadData)[-c(1:2)]
dyadData_toMerge = dyadData[match(did, ddid),toMerge]
data = cbind(data, dyadData_toMerge)
###############################################################

###############################################################
# switch out p5_absidealdiff with alt trade measure
p5Vars = paste0('p5_', vars, 'Min')
data[!is.finite(data[,p5Vars]),p5Vars] = NA
data$lag1_p5_absidealdiffMin = data[,p5Vars]*10
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
  file=paste0(pathResults, 'sobState_statePrefAlt_fin.rda'))
###############################################################