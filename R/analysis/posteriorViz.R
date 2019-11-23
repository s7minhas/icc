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
# summarize
# pathGraphics='~/Desktop/'
# pathResults='~/Desktop/'
# pathResults = '~/Dropbox/forec2/'
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
stateMod = mod

# load(paste0('~/Desktop/sobOpp_model1a_1_newp5Var_hier.rda'))
# oppMod = modHier
# load(paste0('~/Desktop/sobState_model1a_1_newp5Var_hier.rda'))
# stateMod = modHier

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

# create model summaries
## labels for facets
gLab = paste0('Global Effects (',c('Opposition','State'),' Model)')
l1Lab = paste0('No ICC to Prelim Effects (',c('Opposition','State'),' Model)')
l2Lab = paste0('ICC Prelim to Formal Effects (',c('Opposition','State'),' Model)')

# generate pdfs
ggB = ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
## rebel model
rebelSumm = vizWrapper(oppMod, gLab[1], l1Lab[1], l2Lab[1])
rebelViz = arrangeGrob(
	arrangeGrob(ggB,rebelSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(rebelSumm$l1, rebelSumm$l2, ncol=2, widths=c(.55,.45)),	
	nrow=2 )
ggsave(rebelViz, 
	# file=paste0(pathGraphics, 'rebelCoefSumm.pdf'),
	file=paste0('~/Desktop/rebelCoefSumm.pdf'),
	width=8, height=6, device=cairo_pdf)

## state model
stateSumm = vizWrapper(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateViz = arrangeGrob(
	arrangeGrob(ggB,stateSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(stateSumm$l1, stateSumm$l2, ncol=2, widths=c(.55,.45)),	
	nrow=2 )
ggsave(stateViz, 
	file=paste0(pathGraphics, 'stateCoefSumm.pdf'),
	width=8, height=6, device=cairo_pdf)
###############################################################	

###############################################################	
# get out stdz tables
oppTab=stdzTable(oppMod, gLab[1], l1Lab[1], l2Lab[1])
oppRes = lapply(oppTab, function(tab){ apply(tab, 2, mean) })
round(cbind(unlist(oppRes)), 2)
stateTab=stdzTable(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateRes = lapply(stateTab, function(tab){ apply(tab, 2, mean) })
round(cbind(unlist(stateRes)), 2)
###############################################################	