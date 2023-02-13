###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829', 'sminhas')){
	source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('herme','Owner', 'S7M')){
	user <- Sys.info()['user']
	baseDir <- paste0('C:/Users/',Sys.info()['user'],'/')
	source( paste0(baseDir,'Research/icc/R/setup.R') )
	pathGraphics = paste0(baseDir, 'Research/icc/iccPaper/')
}

#
set.seed(6886)
loadPkg(
	c(
		'brms', 'bayesplot', 'MASS', 'VGAM',
		# tadaa is a wrapper around ryourready
		# https://github.com/cran/ryouready/blob/5d76b21f98737ac2778cd207b56309bcc78df9ed/R/association_measures.r
		'tadaatoolbox', 'reshape2', 'RColorBrewer',
		'extrafont', 'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
		)
	)
source(paste0(pathGit, 'R/functions/bayesplot_helpers.R'))
###############################################################

###############################################################
# summarize
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_noImp.rda'))
oppMod <- mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var_noImp.rda'))
stateMod <- mod

# vars
varsRaw <- unique(
	c( rownames(fixef(oppMod)), rownames(fixef(stateMod)) ) )
vars <- unique(gsub('\\[[1-9]\\]','',varsRaw))
varKey <- data.frame(
	dirty = vars, stringsAsFactors = FALSE )
varKey$clean <- c(
	'Intercept',
	'ICC Ratification',
	'Civil War$_{t-1}$',
	'Polity$_{t-1}$',
	'Log(GDP per capita)$_{t-1}$',
	'Africa',
	'Judicial\n Independence$_{t-1}$',
	'Cumulative\n Opp OSV$_{t-1}$',
	'P5 Min. Ideal Pt.$_{t-1}$',
	'Cumulative\n Govt OSV$_{t-1}$'
	)
addCats <- function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey <- rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )

# create model summaries
## labels for facets
gLab <- paste0('Global Effects (',c('Opposition','State'),' Model)')
l1Lab <- paste0('No ICC to Prelim Effects (',c('Opposition','State'),' Model)')
l2Lab <- paste0('ICC Prelim to Formal Effects (',c('Opposition','State'),' Model)')

# helper for plotting
ggB <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()

# trace plots
rebelTrace <- vizWrapper(oppMod, gLab[1], l1Lab[1], l2Lab[1], trace=TRUE)
rebelTraceViz <- arrangeGrob(
	arrangeGrob(ggB,rebelTrace$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(rebelTrace$l1, rebelTrace$l2, ncol=2, widths=c(.5,.5)),
	nrow=2 )
ggsave(rebelTraceViz,
	file=paste0(pathGraphics, 'rebelCoefTrace_noImp.pdf'),
	width=10, height=10, device=cairo_pdf)

stateTrace <- vizWrapper(stateMod, gLab[2], l1Lab[2], l2Lab[2], trace=TRUE)
stateTraceViz <- arrangeGrob(
	arrangeGrob(ggB,stateTrace$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(stateTrace$l1, stateTrace$l2, ncol=2, widths=c(.5,.5)),
	nrow=2 )
ggsave(stateTraceViz,
	file=paste0(pathGraphics, 'stateCoefTrace_noImp.pdf'),
	width=10, height=10, device=cairo_pdf)

## rebel model coef plot
rebelSumm <- vizWrapper(oppMod, gLab[1], l1Lab[1], l2Lab[1])
rebelViz <- arrangeGrob(
	arrangeGrob(ggB,rebelSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(rebelSumm$l1, rebelSumm$l2, ncol=2, widths=c(.55,.45)),
	nrow=2 )
ggsave(rebelViz,
	file=paste0(pathGraphics, 'rebelCoefSumm_noImp.pdf'),
	width=8, height=6, device=cairo_pdf)

## state model
stateSumm <- vizWrapper(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateViz <- arrangeGrob(
	arrangeGrob(ggB,stateSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(stateSumm$l1, stateSumm$l2, ncol=2, widths=c(.55,.45)),
	nrow=2 )
ggsave(stateViz,
	file=paste0(pathGraphics, 'stateCoefSumm_noImp.pdf'),
	width=8, height=6, device=cairo_pdf)
###############################################################

###############################################################
# get out stdz tables
oppTab<-stdzTable(oppMod, gLab[1], l1Lab[1], l2Lab[1])
oppRes <- lapply(oppTab, function(tab){ apply(tab, 2, mean) })
# round(cbind(unlist(oppRes)), 2)
stateTab<-stdzTable(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateRes <- lapply(stateTab, function(tab){ apply(tab, 2, mean) })
# round(cbind(unlist(stateRes)), 2)
###############################################################
