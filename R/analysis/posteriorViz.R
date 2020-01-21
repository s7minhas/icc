###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('herme','Owner')){
	user <- Sys.info()['user']
	baseDir <- paste0('C:/Users/',Sys.info()['user'],'/')
	source( paste0(baseDir,'Research/icc/R/setup.R') )
	pathGraphics = paste0(baseDir, 'Research/icc/iccPaper/')
}

#
loadPkg(
	c(
		'brms', 'bayesplot', 'MASS',
		# tadaa is a wrapper around ryourready
		# https://github.com/cran/ryouready/blob/5d76b21f98737ac2778cd207b56309bcc78df9ed/R/association_measures.r
		'tadaatoolbox',
		'extrafont', 'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
		)
	)
source(paste0(pathGit, 'R/functions/bayesplot_helpers.R'))
###############################################################

###############################################################
# summarize
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
	'P5 Min. Ideal Pt.$_{t-1}$',
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
ggB = ggplot()+geom_blank(aes(1,1
                              )) + cowplot::theme_nothing()
## rebel model
rebelSumm = vizWrapper(oppMod, gLab[1], l1Lab[1], l2Lab[1])
rebelViz = arrangeGrob(
	arrangeGrob(ggB,rebelSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(rebelSumm$l1, rebelSumm$l2, ncol=2, widths=c(.55,.45)),
	nrow=2 )
ggsave(rebelViz,
	file=paste0(pathGraphics, 'rebelCoefSumm.pdf'),
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
# round(cbind(unlist(oppRes)), 2)
stateTab=stdzTable(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateRes = lapply(stateTab, function(tab){ apply(tab, 2, mean) })
# round(cbind(unlist(stateRes)), 2)
###############################################################

###############################################################
# perf analysis
x = predict(oppMod)
data = oppMod$data
y = data$icclevel_opp_3
predDF = data.frame(cbind(x, y), stringsAsFactors=FALSE)
predDF$guess = apply(predDF[,1:3], 1, function(x){
	which(x==max(x)) })

# compare with polr
dv = as.character(oppMod$formula$formula)[2]
ivs = as.character(oppMod$formula$formula)[3]
ivs = gsub(')','',gsub('cs(','',ivs,fixed=TRUE),fixed=TRUE)
polrForm = formula( paste0(dv, '~', ivs) )
data[,dv] = factor(
	data[,dv],
	ordered=TRUE,
	levels=sort(unique(data[,dv]))
)
base = polr(polrForm, data=data, Hess=TRUE)
predDF$polrPreds = predict(base)

ord_somers_d(predDF$y, predDF$guess)
ord_somers_d(predDF$y, predDF$polrPreds)
tadaa_ord(predDF$y, predDF$guess)
###############################################################
