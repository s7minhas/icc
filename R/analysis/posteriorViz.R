###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('brms','latex2exp','bayesplot'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
stateMod = mod

# vars
vars = unique(
	c(
		rownames(fixef(oppMod)), 
		rownames(fixef(stateMod)))
	)
vars = unique(gsub('\\[[1-9]\\]','',vars))
varKey = data.frame(
	dirty = vars, stringsAsFactors = FALSE )
varKey$clean = c(
	'Intercept',
	'ICC Ratification',
	'Civil War$_{t-1}$',
	'Polity$_{t-1}$',
	'Log(GDP per capita)$_{t-1}$',
	'Africa',
	'Judicial\n Independence$_{t-1}$',
	'Cumulative\n Rebel OSV$_{t-1}$',
	'P5 Closeness$_{t-1}$',
	'Cumulative\n Govt. OSV$_{t-1}$'
	)

# viz
oppBeta = data.frame(
	fixef(oppMod, summary=FALSE),
	stringsAsFactors = FALSE
	)

# org vars
gVars = colnames(oppBeta)[
	!grepl('.',colnames(oppBeta), fixed=TRUE) ]
l1Vars = colnames(oppBeta)[
	grepl('.1.',colnames(oppBeta), fixed=TRUE) ]
l2Vars = colnames(oppBeta)[
	grepl('.2.',colnames(oppBeta), fixed=TRUE) ]

# org data for plots
gModBeta = oppBeta[,gVars]
l1ModBeta = oppBeta[,l1Vars]
l2ModBeta = oppBeta[,l2Vars]

# stdz vars
stdzCoef = function(coefVar, baseVar, dv){
	return( coefVar * (sd(baseVar)/sd(dv)) ) }	
vars = colnames(gModBeta)[-ncol(gModBeta)]
for(v in vars){
	gModBeta[,v] = stdzCoef(
		gModBeta[,v], 
		oppMod$data[,v],
		oppMod$data$icclevel_opp_3) }

# viz
# mcmc_areas_data	
varLabs = varKey$clean
names(varLabs) = varKey$dirty

color_scheme_set("gray")
mcmc_areas(
	gModBeta,
	pars = colnames(gModBeta),
	prob = 0.95, prob_outer=1,
	point_est = 'mean'
	) +
	geom_vline(
		aes(xintercept=0), 
		color='black', linetype='dashed'
		) +
	# scale_y_discrete('', labels=varLabs) +
	scale_y_discrete('', labels=TeX(varLabs)) +
 	theme_bw() +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank()
		)
###############################################################