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
	'lag1_poi_osv_rebel',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyMax',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
data$lag1_poi_osv_rebel = log(data$lag1_poi_osv_rebel+1)
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
frame = cbind(data[,c('ccode','year','icclevel2_opp_3a')], frame)
frame$icclevel2_opp_3a = as.integer(frame$icclevel2_opp_3a)
frame$ccode = as.integer(frame$ccode)
frame = na.omit(frame)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year','icclevel2_opp_3a')], x)
	names(x) = names(frame)
	x$icclevel2_opp_3a = as.integer(x$icclevel2_opp_3a)
	x$ccode = as.integer(x$ccode)
	x = na.omit(x)
	return(x) })
###############################################################

###############################################################
# category specific effects
sobOppVars[c(5,7,8)] = paste0('cs(',sobOppVars[c(5,7,8)],')')

# pool
sobOppForm = formula(
	paste0('icclevel2_opp_3a ~ ', 
		paste(sobOppVars, collapse = ' + ') ) )
mod = brm(
	formula=sobOppForm, 
	data=frame,
	family=cratio(link='logit')
	)	
save(mod, file=paste0(pathResults, 'sobOpp_model2a.rda'))

# hier
sobOppForm = formula(
	paste0('icclevel2_opp_3a ~ ', 
		paste(sobOppVars, collapse = ' + '), '+(1|ccode)' ) )
modHier = brm(
	formula=sobOppForm, 
	data=frame,
	family=cratio(link='logit')
	)
save(modHier, file=paste0(pathResults, 'sobOpp_model2a_hier.rda'))
###############################################################