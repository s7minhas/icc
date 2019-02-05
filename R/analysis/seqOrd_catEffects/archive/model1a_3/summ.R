###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_3.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_3.rda'))
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
	sobVars[c(1:4,11:13)],
	sort(
		pasteMult(
			sobVars[c(5:10)], paste0('[',1:2,']'),
			sepZ='')
		)
	)

#
sobVars = sobVars[-c(5:7)]
sobVars = c(sobVars[1:4],'lag1_p5_intv',sobVars[5:length(sobVars)])

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
	file=paste0(pathResults, 'sob_model1a_3.tex'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_3_hier.rda'))
oppMod = modHier
load(paste0(pathResults, 'sobState_model1a_3_hier.rda'))
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
	file=paste0(pathResults, 'sob_model1a_3_hier.tex'))
###############################################################