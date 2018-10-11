###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
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
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyMax',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'sobOpp_imp.rda'))){
	toImp = data.matrix(data[,c('icclevel_opp',sobOppVars)])
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'sobOpp_imp.rda'))
} else { load(paste0(pathData, 'sobOpp_imp.rda')) }

# pick a few from the posterior
set.seed(6886)
frame = data.frame(impData$Y.pmean)
frame = cbind(data[,c('ccode','year','icclevel_opp_3')], frame)
frame$icclevel_opp_3 = as.integer(frame$icclevel_opp_3 + 1)
frame$ccode = as.integer(frame$ccode)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year','icclevel_opp_3')], x)
	names(x) = names(frame)
	x$icclevel_opp_3 = as.integer(x$icclevel_opp_3 + 1)
	x$ccode = as.integer(x$ccode)
	return(x) })
###############################################################

###############################################################
# category specific effects
sobOppVars[c(5:11)] = paste0('cs(',sobOppVars[c(5:11)],')')

# pool
if(!file.exists(paste0(pathResults, 'sobOpp_model1a.rda'))){
	sobOppForm = formula(
		paste0('icclevel_opp_3 ~ ', 
			paste(sobOppVars, collapse = ' + ') ) )
	mod = brm(
		formula=sobOppForm, 
		data=frame,
		family=cratio(link='logit')
		)	
	save(mod, file=paste0(pathResults, 'sobOpp_model1a.rda'))
} else {load(paste0(pathResults, 'sobOpp_model1a.rda'))}

# hier
if(!file.exists(paste0(pathResults, 'sobOpp_model1a_hier.rda'))){
	sobOppForm = formula(
		paste0('icclevel_opp_3 ~ ', 
			paste(sobOppVars, collapse = ' + '), '+(1|ccode)' ) )
	modHier = brm(
		formula=sobOppForm, 
		data=frame,
		family=cratio(link='logit')
		)
	save(modHier, file=paste0(pathResults, 'sobOpp_model1a_hier.rda'))
} else {load(paste0(pathResults, 'sobOpp_model1a_hier.rda'))}
###############################################################