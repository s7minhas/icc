###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('brms'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
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
sobVars = sobVars[-c(5:7,16:17)]

#
varKey = data.frame(dirty=sobVars, stringsAsFactors = FALSE)
varKey$clean = c(
	'ICC Ratification',
	'Civil War$_{i,t-1}$',
	'Polity$_{i,t-1}$',
	'Log(GDP per capita)$_{i,t-1}$',
	'Africa'
	)

# 
x = data.frame(sobMods$'state')
x$dirty = rownames(x) ; rownames(x) = NULL
# x$clean = c()

# table
oppBeta = fixef(oppMod, summary=FALSE)

# marginal effects
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
x = marginal_effects(
	oppMod, 
	effects='lag1_v2juncind', 
	categorical=TRUE)[[1]]

ggplot(data=x, 
	aes(
		x=lag1_v2juncind, y=estimate__, 
		color=cats__, fill=cats__
		)) + 
	geom_line() + 
	geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=.5) + 
	geom_rug(data=data, aes(x=lag1_v2juncind,y=0),sides='b') +
	facet_wrap(~cats__, scales='free_y')
###############################################################