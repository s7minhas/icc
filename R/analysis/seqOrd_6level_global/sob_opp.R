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
frame = cbind(data[,c('ccode','year')], frame)
frame$icclevel_opp = as.integer(frame$icclevel_opp + 1)
frame$ccode = as.integer(frame$ccode)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year')], x)
	names(x) = names(frame)
	x$icclevel_opp = as.integer(x$icclevel_opp + 1)
	x$ccode = as.integer(x$ccode)
	return(x) })
###############################################################

###############################################################
# pool
if(!file.exists(paste0(pathResults, 'sobOpp_mod.rda'))){
	sobOppForm = formula(
		paste0('icclevel_opp ~ ', 
			paste(sobOppVars, collapse = ' + ') ) )
	mod = brm(
		formula=sobOppForm, 
		data=frame,
		family=cratio(link='logit')
		)	
	save(mod, file=paste0(pathResults, 'sobOpp_mod.rda'))
} else {load(paste0(pathResults, 'sobOpp_mod.rda'))}

# hier
if(!file.exists(paste0(pathResults, 'sobOpp_mod_hier.rda'))){
	sobOppForm = formula(
		paste0('icclevel_opp ~ ', 
			paste(sobOppVars, collapse = ' + '), '+(1|ccode)' ) )
	modHier = brm(
		formula=sobOppForm, 
		data=frame,
		family=cratio(link='logit')
		)
	save(modHier, file=paste0(pathResults, 'sobOpp_mod_hier.rda'))
} else {load(paste0(pathResults, 'sobOpp_mod_hier.rda'))}

# summarize
oppMod = mod
load(paste0(pathResults, 'sobState_mod.rda'))
stateMod = mod
sobMods = lapply(list(stateMod, oppMod), function(x){
	summ=data.frame(fixef(x)[,1:2])
	names(summ) = c('beta','serror')
	summ$z = summ$beta/summ$serror
	return(summ) })

# vars
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
sobVars = unique(c(sobStateVars, sobOppVars))
sobVars = sobVars[c(1:8,13,9:12)]

# table
# clean table
lazyCleanVars = gsub('_',' ',sobVars,fixed=TRUE)
lazyCleanMods = c('state','rebel')
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(sobVars,lazyCleanVars,sobMods,lazyCleanMods)
print.xtable(xtable(res, align='llcc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(sobVars)*2,length(sobVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'sob.tex'))
###############################################################