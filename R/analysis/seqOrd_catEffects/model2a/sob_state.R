###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobStateVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind','lag1_poi_pts',
	'lag1_poi_osv_state',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyMax',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'sobState_imp.rda'))){
	toImp = data.matrix(data[,c('icclevel_state',sobStateVars)])
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'sobState_imp.rda'))
} else { load(paste0(pathData, 'sobState_imp.rda')) }

# pick a few from the posterior
set.seed(6886)
frame = data.frame(impData$Y.pmean)
frame = cbind(data[,c('ccode','year','icclevel2_state_3a')], frame)
frame$icclevel2_state_3a = as.integer(frame$icclevel2_state_3a)
frame$ccode = as.integer(frame$ccode)
frame = na.omit(frame)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year','icclevel2_state_3a')], x)
	names(x) = names(frame)
	x$icclevel2_state_3a = as.integer(x$icclevel2_state_3a)
	x$ccode = as.integer(x$ccode)
	x = na.omit(x)
	return(x) })
###############################################################

###############################################################
# category specific effects
sobStateVars[c(5,8,9)] = paste0('cs(',sobStateVars[c(5,8,9)],')')

# pool
sobStateForm = formula(
	paste0('icclevel2_state_3a ~ ', 
		paste(sobStateVars, collapse = ' + ') ) )
mod = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit')
	)
save(mod, file=paste0(pathResults, 'sobState_model2a.rda'))

# hier
sobStateForm = formula(
	paste0('icclevel2_state_3a ~ ', 
		paste(sobStateVars, collapse = ' + '), '+(1|ccode)' ) )
modHier = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit')
	)
save(modHier, file=paste0(pathResults, 'sobState_model2a_hier.rda'))
###############################################################