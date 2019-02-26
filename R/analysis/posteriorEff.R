###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(
	c(
		'brms', 'bayesplot',
		'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
		) 
	)
source(paste0(pathGit, 'R/functions/bayesplot_helpers.R'))
###############################################################

###############################################################
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
stateMod = mod

# vars
varsRaw = unique(
	c( rownames(fixef(oppMod)), rownames(fixef(stateMod)) ) )
vars = unique(gsub('\\[[1-9]\\]','',varsRaw))
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
	'Cumulative\n Opp OSV$_{t-1}$',
	'P5 Closeness$_{t-1}$',
	'Cumulative\n Govt OSV$_{t-1}$'
	)
addCats = function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey = rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )
###############################################################

###############################################################
# marginal effects
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

var = 'lag1_p5_absidealdiffMin'
x = marginal_effects(
	oppMod, 
	effects=var, robust=TRUE,
	probs=c(0.05,0.95),
	method='fitted',
	categorical=TRUE, 
	plot=FALSE)[[1]]

names(x)[1] = 'voi'
ggplot(data=x, 
	aes(
		x=voi, y=estimate__, 
		color=cats__, fill=cats__
		)) + 
	geom_line() + 
	geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=.5) + 
	# geom_rug(data=data, aes(x=lag1_v2juncind,y=0),sides='b') +
	facet_wrap(~cats__, scales='free_y')
###############################################################	