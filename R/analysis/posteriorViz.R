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

# viz
oppBeta = data.frame(
	fixef(oppMod, summary=FALSE),
	stringsAsFactors = FALSE
	)
oppBeta$iteration = 1:nrow(oppBeta)

# org vars
gVars = colnames(oppBeta)[
	!grepl('.',colnames(oppBeta), fixed=TRUE) ]
l1Vars = colnames(oppBeta)[
	grepl('.1.',colnames(oppBeta), fixed=TRUE) ]
l2Vars = colnames(oppBeta)[
	grepl('.2.',colnames(oppBeta), fixed=TRUE) ]

# org data for plots
gOppBeta = oppBeta[,gVars]
l1OppBeta = oppBeta[,l1Vars]
l2OppBeta = oppBeta[,l2Vars]

# stdz vars
stdzCoef = function(coefVar, baseVar, dv){
	return( coefVar * (sd(baseVar)/sd(dv)) ) }	
vars = colnames(gOppBeta)[-ncol(gOppBeta)]
for(v in vars){
	gOppBeta[,v] = stdzCoef(
		gOppBeta[,v], 
		oppMod$data[,v],
		oppMod$data$icclevel_opp_3) }

# viz
# mcmc_areas_data	
gOppBeta = gOppBeta[,-ncol(gOppBeta)]

color_scheme_set("gray")
mcmc_areas(
	gOppBeta,
	pars = colnames(gOppBeta),
	prob = 0.95, prob_outer=1,
	point_est = 'mean'
	) +
	geom_vline(aes(xintercept=0), 
		color='black', linetype='dashed'
		) +
	theme_bw() +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank()
		)
###############################################################