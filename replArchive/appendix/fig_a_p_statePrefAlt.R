###############################################################
source(paste0(here::here(), '/setup.R'))

#
loadPkg(
	c(
		'brms', 'bayesplot', 'extrafont',
		'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
		)
	)
source(paste0(pathFuncs, 'bayesplot_helpers.R'))
###############################################################

###############################################################
# load in robustness check data
modvar = 'trade'  
load(paste0(pathResults, 'sobOpp_statePrefAlt_fin.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_statePrefAlt_fin.rda'))
stateMod = mod
###############################################################

###############################################################
# vars
varsRaw = unique(
	c( rownames(fixef(oppMod)), rownames(fixef(stateMod)) ) )
vars = unique(gsub('\\[[1-9]\\]','',varsRaw))
varKey = data.frame(
	dirty = vars, stringsAsFactors = FALSE )
varKey$clean = c(
	'Intercept',
	'ICC Ratification',
	'Civil War',	
	'Polity',
	'Log(GDP per capita)',
	'Africa',
	'Judicial\n Independence',
	'Cumulative\n Opp OSV',
	'P5 Trade',
	'Cumulative\n Govt OSV'
	)

# reorder so osv comes before p5 measure
varKey = varKey[c(1:8,10,9),]
# add stage versions	
addCats = function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey = rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )
###############################################################	

###############################################################
viz = coef_grid(
	oppMod, 'Opposition-Focused',
	stateMod, 'State-Focused' )
###############################################################

###############################################################
# save
ggsave(viz,
	file=paste0(pathGraphics, 'fig_a_p_statePrefAlt.png'),
	width=8, height=6, dpi=600)
###############################################################