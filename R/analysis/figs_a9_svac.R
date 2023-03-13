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
load(paste0(pathResults, 'sobOpp_svac_fin.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_svac_fin.rda'))
stateMod = mod

load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_fin.rda'))
oppModOG = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var_fin.rda'))
stateModOG = mod
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
	'Cumulative\n Opp SVAC',
	'P5 Min. Ideal Pt.',
	'Cumulative\n Govt SVAC'
	)
addCats = function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey = rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )
###############################################################	

###############################################################
# get summary of coef vals
coefData = rbind(
	stanSumm(oppMod, 'Opposition-Focused'),
	stanSumm(stateMod, 'State-Focused') )

coefDataOG = rbind(
	stanSumm(oppModOG, 'Opposition-Focused'),
	stanSumm(stateModOG, 'State-Focused') )

coefData$tst = 'Robustness Check'
coefDataOG$tst = 'Original Model'

coefData = rbind(coefData, coefDataOG)

# reorder vars
coefData$stage = factor( coefData$stage, levels=c(
		'Global Effects',
		'No ICC to\nPrelim Effects',
		'ICC Prelim to\nFormal Effects' ))
coefData$clean = factor(coefData$clean, levels=rev(unique(varKey$clean[-1])))

# make coef plot
viz=ggplot(
	data=coefData, 
	aes(x=clean, y=Estimate, color=tst)) +
	geom_point(position=position_dodge(.5)) +
	geom_errorbar( aes(ymin=Q2.5, ymax=Q97.5), width=.1, position=position_dodge(.5)) +
	geom_hline(yintercept=0, linetype='dashed', color='grey') +
	labs(
		x='', y='', color='' ) +
	coord_flip() +
	facet_grid(stage~type, scales='free') +
	theme_bw() +
	theme(
		# panel.border=element_blank(),
		legend.position='top',
		axis.ticks=element_blank(),
		strip.text = element_text(size = 9, color='white',
			family="Source Sans Pro SemiBold", 
			angle=0, hjust=.05),
		strip.background = element_rect(fill = "#525252", color='#525252') )
print(viz)
###############################################################

###############################################################
# save
# ggsave(stateViz,
# 	file=paste0(pathGraphics, 'fig_a7.pdf'),
# 	width=8, height=6, device=cairo_pdf)
###############################################################
