###############################################################
source('~/Research/icc/R/setup.R')

#
loadPkg(
	c(
		'brms', 'bayesplot', 'extrafont',
		'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
		)
	)
source(paste0(pathGit, 'R/functions/bayesplot_helpers.R'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda'))
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
	'Polity',
	'Log(GDP per capita)',
	'Africa',
	'Judicial\n Independence',
	'Cumulative\n Opp OSV',
	'P5 Min. Ideal Pt.',
	'Cumulative\n Govt OSV'
	)
# reorder so osv comes before p5 measure
varKey = varKey[c(1:7,9,8),]
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
	file=paste0(pathGraphics, 'fig_a_ptsCivilWarOnly.pdf'),
	width=8, height=7, device=cairo_pdf)
###############################################################