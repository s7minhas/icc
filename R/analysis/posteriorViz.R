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
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod = mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
stateMod = mod

# vars
varsRaw = unique(
	c(
		rownames(fixef(oppMod)), 
		rownames(fixef(stateMod)))
	)
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
	'Cumulative\n Rebel OSV$_{t-1}$',
	'P5 Closeness$_{t-1}$',
	'Cumulative\n Govt. OSV$_{t-1}$'
	)
addCats = function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey = rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )

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

# viz
varLabs = varKey$clean
names(varLabs) = varKey$dirty
ggGlobal = mcmcViz(
	prepData(oppBeta[,gVars], 
		'Global Effects'), varLabs)
ggLevel1 = mcmcViz(
	prepData(oppBeta[,l1Vars[-1]], 
		'No ICC to Prelim Effects'), varLabs)
ggLevel2 = mcmcViz(
	prepData(oppBeta[,l2Vars[-1]], 
		'ICC Prelim to Formal Effects'), varLabs) +
	theme(
		axis.text.y = element_blank()
		)

#
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

grid.arrange(
	arrangeGrob(blankPlot,ggGlobal,blankPlot, widths=c(.1,.7,.2)),
	arrangeGrob(ggLevel1, ggLevel2, ncol=2, widths=c(.55,.45)),	
	nrow=2
	)
###############################################################	