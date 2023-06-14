###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop', 'brms', 'future'))
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
  'lag1_p5_absidealdiffMin' )

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
###############################################################

###############################################################
# read in sexual violence data from svac
svac = read.csv(paste0(pathData, 'SVAC_3.0_complete.csv'))

# subset to years of our study
svac = svac[svac$year > 2000,]

# subset to cases where sexual violence occurred
# using data from state dept
svac = svac[which(svac$state_prev>0),]

# generate state level measure of sexual violence
# subset to cases where actor was clearly government
svac = svac[which(!svac$actor_type %in% c(3,6)),]

# match countrynames
svac$actor[
  svac$actor=='DR Congo (Zaire)'
  ] = 'Congo, Democratic Republic of the'
svac$cname = cname(svac$actor)

# create cumulative version
svac = lapply(unique(svac$cname), function(x){
	slice = svac[which(svac$cname==x),]
	slice = slice[order(slice$year),]
	slice$prev_cumul = cumsum(slice$state_prev)	
	return(slice) }) %>% do.call('rbind', .)

# create id for matching, lag data, 
# and merge prev score into data object
svac$year = svac$year + 1
svac$cnameYear = paste(svac$cname, svac$year, sep='_')
data$lag1_osv_state_cumul = svac$prev_cumul[
  match(data$cnameYear,svac$cnameYear)]

# log and turn NAs to 0
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
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
  file=paste0(pathResults, 'sobState_svac_fin.rda'))
###############################################################