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
	'lag1_p5_defAllyAvg',
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
frame = cbind(data[,c('ccode','year')], frame)
frame$icclevel_state = as.integer(frame$icclevel_state + 1)
frame$ccode = as.integer(frame$ccode)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year')], x)
	names(x) = names(frame)
	x$icclevel_state = as.integer(x$icclevel_state + 1)
	x$ccode = as.integer(x$ccode)
	return(x) })
###############################################################

###############################################################
# pool
if(!file.exists(paste0(pathResults, 'sobState_mod.rda'))){
	sobStateForm = formula(
		paste0('icclevel_state ~ ', 
			paste(sobStateVars, collapse = ' + ') ) )
	mod = brm(
		formula=sobStateForm, 
		data=frame,
		family=cratio(link='logit')
		)
	save(mod, file=paste0(pathResults, 'sobState_mod.rda'))
} else {load(paste0(pathResults, 'sobState_mod.rda'))}

# hier
if(!file.exists(paste0(pathResults, 'sobState_mod_hier.rda'))){
	sobStateForm = formula(
		paste0('icclevel_state ~ ', 
			paste(sobStateVars, collapse = ' + '), '+(1|ccode)' ) )
	modHier = brm(
		formula=sobStateForm, 
		data=frame,
		family=cratio(link='logit')
		)
	save(modHier, file=paste0(pathResults, 'sobState_mod_hier.rda'))
} else {load(paste0(pathResults, 'sobState_mod_hier.rda'))}

# summarize
sobStateMods = lapply(list(mod, modHier), function(x){
	summ=data.frame(fixef(x)[,1:2])
	names(summ) = c('beta','serror')
	summ$z = summ$beta/summ$serror
	return(summ) })

# table
# clean table
lazyCleanVars = gsub('_','',sobStateVars,fixed=TRUE)
lazyCleanMods = c('m1')
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(sobStateVars,lazyCleanVars,sobStateMods[[1]],lazyCleanMods)
print.xtable(xtable(res, align='llcc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(sobStateVars)*2,length(sobStateVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'sobState.tex'))
###############################################################