###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_4_global.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_4_global.rda'))
stateMod = mod
sobMods = lapply(list(stateMod, oppMod), function(x){
	summ=data.frame(fixef(x)[,1:2])
	names(summ) = c('beta','serror')
	summ$z = summ$beta/summ$serror
	return(summ) })
names(sobMods) = c('state', 'opp')

# vars
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
sobVars = unique(c(sobStateVars, sobOppVars))
sobVars = sobVars[c(1:8,13,9:12)]
sobVars = c(
	sobVars[c(1:4,6:7,11:13)],
	sobVars[c(5,8:10)]
	)

#
sobVars = sobVars[-c(7:9)]
sobVars = c(sobVars, 'lag1_p5_intv')

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
	file=paste0(pathResults, 'sob_model1a_4_global.tex'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_4_global_hier.rda'))
oppMod = modHier
load(paste0(pathResults, 'sobState_model1a_4_global_hier.rda'))
stateMod = modHier
sobMods = lapply(list(stateMod, oppMod), function(x){
	summ=data.frame(fixef(x)[,1:2])
	names(summ) = c('beta','serror')
	summ$z = summ$beta/summ$serror
	return(summ) })
names(sobMods) = c('state', 'opp')

# table
# clean table
res=getTable(sobVars,lazyCleanVars,sobMods,lazyCleanMods)
print.xtable(xtable(res, align='llcc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(sobVars)*2,length(sobVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'sob_model1a_4_global_hier.tex'))
###############################################################