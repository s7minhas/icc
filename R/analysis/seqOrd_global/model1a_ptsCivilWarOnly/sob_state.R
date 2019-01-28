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
	'lag1_v2juncind','lag1_pts',
	'lag1_osv_state_cumul',	
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
frame = cbind(data[,c('ccode','year','icclevel_state_3')], frame)
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
frame$ccode = as.integer(frame$ccode)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year','icclevel_state_3')], x)
	names(x) = names(frame)
	x$icclevel_state_3 = as.integer(x$icclevel_state_3 + 1)
	x$ccode = as.integer(x$ccode)
	return(x) })
###############################################################

###############################################################
# only incl states with civ war or pts>=3 since 2002
load(paste0(pathData, 'subset_ptsCivWar_cntries.rda'))

## keep only states 
frame = frame[which(frame$ccode %in% cntries$ccode),]

# remove civl war covariate
sobStateVars = sobStateVars[-which(sobStateVars %in% c('lag1_civilwar'))]
###############################################################

###############################################################
# pool
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + ') ) )
mod = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit')
	)
save(mod, 
	file=paste0(pathResults, 'sobState_model1a_global_ptsCivilWarOnly.rda'))

# hier
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + '), '+(1|ccode)' ) )
modHier = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit')
	)
save(modHier, 
	file=paste0(pathResults, 'sobState_model1a_global_hier_ptsCivilWarOnly.rda'))
###############################################################