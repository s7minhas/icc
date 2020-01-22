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
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var.rda'))
oppMod <- mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
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

# generate pdfs
ggB <- ggplot()+geom_blank(aes(1,1
                              )) + cowplot::theme_nothing()
## rebel model
rebelSumm <- vizWrapper(oppMod, gLab[1], l1Lab[1], l2Lab[1])
rebelViz <- arrangeGrob(
	arrangeGrob(ggB,rebelSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(rebelSumm$l1, rebelSumm$l2, ncol=2, widths=c(.55,.45)),
	nrow=2 )
ggsave(rebelViz,
	file=paste0(pathGraphics, 'rebelCoefSumm.pdf'),
	width=8, height=6, device=cairo_pdf)

## state model
stateSumm <- vizWrapper(stateMod, gLab[2], l1Lab[2], l2Lab[2])
stateViz <- arrangeGrob(
	arrangeGrob(ggB,stateSumm$g,ggB, widths=c(.15,.65,.2)),
	arrangeGrob(stateSumm$l1, stateSumm$l2, ncol=2, widths=c(.55,.45)),
	nrow=2 )
ggsave(stateViz,
	file=paste0(pathGraphics, 'stateCoefSumm.pdf'),
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

###############################################################
# perf analysis
getPerfStats <- function(mod, lab){

	# extract predictions
	x <- predict(mod)
	data <- mod$data
	y <- data[,1]
	predDF <- data.frame(cbind(x, y), stringsAsFactors=FALSE)
	# pick outcome based on max prob assignment
	predDF$guess <- apply(predDF[,1:3], 1,
		function(x){
			which(x==max(x)) })

	## compare with polr
	# get form for polr from mod
	dv <- as.character(mod$formula$formula)[2]
	ivs <- as.character(mod$formula$formula)[3]
	ivs <- gsub(')','',gsub('cs(','',ivs,fixed=TRUE),fixed=TRUE)
	polrForm <- formula( paste0(dv, '~', ivs) )

	# make sure dv is structured correctly
	data[,dv] = factor(
		data[,dv],
		ordered=TRUE,
		levels=sort(unique(data[,dv]))
	)

	# run polr
	base <- polr(polrForm, data=data, Hess=TRUE)
	baseSeq <- vglm(
		polrForm, data=data,
		family = cumulative (link="probit", parallel=TRUE))
	# extract preds
	predDF$polrPreds = predict(base)

	# run sequential model with no cs terms
	baseSeq <- vglm(
		polrForm, data=data,
		family = sratio (link="logit", parallel=TRUE))
	baseSeqPreds <- predict(baseSeq, type='res')
	predDF$vglmPreds <- apply(baseSeqPreds[,1:3], 1,
		function(x){
			which(x==max(x)) })

	# get out perf stats
	out <- rbind(
		cbind(
			tadaa_ord(
				predDF$y, predDF$guess
				)$body[,c('value','col_name')],
			lab=lab, model='Sequential Model' ),
		cbind(
			tadaa_ord(
				predDF$y, predDF$vglmPreds
				)$body[,c('value','col_name')],
			lab=lab, model='Ordinal Model' ),
		cbind(
			tadaa_ord(
				predDF$y, predDF$polrPreds
				)$body[,c('value','col_name')],
			lab=lab, model='Sequential Model\nNo Category Specific' )
	)

	return(out) }

# org
if(!file.exists(paste0(pathResults, 'perfResults.rda'))){
  perfData <- rbind(
    getPerfStats(oppMod, lab='Opposition-Focused'),
    getPerfStats(stateMod, lab='State-Focused')
  )
  save(perfData, file=paste0(pathResults, 'perfResults.rda'))
} else { load(paste0(pathResults, 'perfResults.rda')) }

# fix up some labels
perfVarKey <- data.frame(dirty=unique(perfData$col_name))
perfVarKey$clean <- c(
	'Goodman-Kruskal\nGamma',
	"Somers' D",
	rep(NA,2),
	"Kendall's Tau-a",
	rep(NA,2) )

# merge cleaned labels and subset
perfData$varName <- perfVarKey$clean[match(perfVarKey$dirty, perfData$col_name)]
perfData <- perfData[!is.na(perfData$varName),]

# ordering of data
perfData$lab <- factor(perfData$lab,
	levels=c('State-Focused', 'Opposition-Focused'))
perfData$model <- factor(perfData$model,
		levels=rev(c(
			'Sequential Model',
			'Sequential Model\nNo Category Specific',
			'Ordinal Model')))
perfData$value <- num(perfData$value)

# plot
cols <- rev(brewer.pal(3, 'Set1'))
perfData = perfData[perfData$col_name %in% c('somer_x','gamma'),]
ggplot(perfData, aes(x=varName, y=value, color=model)) +
	geom_point(position = position_dodge(width = 0.5), size=2) +
	geom_linerange(
		aes(ymin=0, ymax=value), size=1,
		position = position_dodge(width = 0.5)) +
	# scale_y_continuous(breaks=seq(0,1,.1)) +
	scale_color_manual(values=rev(cols)) +
	coord_flip() +
	facet_wrap(~lab) +
	labs(x='', y='') +
	theme(
		axis.ticks=element_blank(),
		panel.border=element_blank(),
		legend.title=element_blank(),
		legend.position='top',
		panel.grid.minor=element_blank()
	)

# focus on somers d
cols <- rev(brewer.pal(3, 'Set1'))
perfData2 = perfData[perfData$col_name %in% c('somer_x'),]
somerViz = ggplot(perfData2, aes(x=model, y=value)) +
  geom_bar(stat='identity') +
  scale_color_manual(values=rev(cols)) +
  coord_flip() +
  facet_wrap(~lab) +
  labs(x='', y="") +
  theme(
    axis.ticks=element_blank(),
    panel.border=element_blank(),
    legend.title=element_blank(),
    legend.position='top',
    panel.grid.minor=element_blank(),
    axis.text.x = element_text(family='Source Sans Pro Light'),
    # axis.text.y = element_text(family='Source Sans Pro Light'),
    strip.text.x = element_text(size = 9, color='white',
                                family="Source Sans Pro SemiBold",
                                angle=0, hjust=.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  )
ggsave(somerViz, file=paste0(pathGraphics, 'somerViz.pdf'),
       width=6, height=3)
###############################################################
