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
# load in robustness check data
allVars = c(
  # 'ptaCnt', # sig neg in prelim and pos but unsig in formal
  # 'allyTotal', # sig neg in prelim and pos but unsig in formal
  # 'matlConfGov', # neg in prelim and pos but basically zero and unsig in formal
  # 'matlCoopGov', # conflicting and confusing results
  # 'verbCoopGov', # conflicting results, not sig in formal
  # 'verbConfGov', # neg across but insig in formal
  #irrelev var: 'tradeBal', # insig in prelim, and pos and sig in formal
  'importsCIF', # insig in prelim and neg and sig in formal
  # 'exportsCIF', # conflicting results not sig
  'importsFOB', # insig in prelim and neg and sig in formal
  # 'exportsFOB', # conflicting results not sig
  'trade', # insig in prelim, neg and sig in formal
  #irrelev var: 'tradeSend'  # insig in prelim and neg and sig in formal
  # 'tradeDepSend', # not sig in formal
  # 'tradeGDP' # not sig in formal
)

plots = list()
for(ii in 1:length(allVars)){

modvar = allVars[ii]
  
load(paste0(pathResults, 'sobOpp_statePrefAlt_', modvar, '_fin.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_statePrefAlt_', modvar, '_fin.rda'))
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
	'P5 Alt Msr',
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
# ggsave(viz,
# 	file=paste0(pathGraphics, 'fig_a_p_statePrefAlt.pdf'),
# 	width=8, height=6, device=cairo_pdf)
###############################################################

plots[[ii]] = viz

}

names(plots) = allVars