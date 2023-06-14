###############################################################
source('~/Research/icc/R/setup.R')

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
# load data
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_noImp_fin.rda'))
oppMod <- mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var_noImp_fin.rda'))
stateMod <- mod
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
		levels=sort(unique(data[,dv])) )

	# run polr
	base <- polr(polrForm, data=data, Hess=TRUE)
	baseSeq <- vglm(
		polrForm, data=data,
		family = cumulative (link="probitlink", parallel=TRUE))
	# extract preds
	predDF$polrPreds = predict(base)

	# run Partial\nContinuation Ratio with no cs terms
	baseSeq <- vglm(
		polrForm, data=data,
		family = sratio (link="logitlink", parallel=TRUE))
	baseSeqPreds <- predict(baseSeq, type='res')
	if(lab=='State-Focused'){
		tmp = apply(baseSeqPreds[,2:3], 1, sum)
		baseSeqPreds = cbind(baseSeqPreds[,1], tmp) }
	predDF$vglmPreds <- apply(baseSeqPreds, 1,
		function(x){
			which(x==max(x)) })

	# get out perf stats
	out <- rbind(
		cbind(
			tadaa_ord(
				predDF$y, predDF$guess
				)$body[,c('value','col_name')],
			lab=lab, model='Partial\nContinuation Ratio' ),
		cbind(
			tadaa_ord(
				predDF$y, predDF$vglmPreds
				)$body[,c('value','col_name')],
			lab=lab, model='Ordinal Model' ),
		cbind(
			tadaa_ord(
				predDF$y, predDF$polrPreds
				)$body[,c('value','col_name')],
			lab=lab, model='Continuation Ratio' )
	)

    #
	return(out) }

# org
if(!file.exists(paste0(pathResults, 'perfResults_fin.rda'))){
    operf=getPerfStats(oppMod, lab='Opposition-Focused')
    sperf=getPerfStats(stateMod, lab='State-Focused')
    perfData <- rbind(operf, sperf)
    save(perfData, file=paste0(pathResults, 'perfResults_fin.rda'))
} else { load(paste0(pathResults, 'perfResults_fin.rda')) }

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
			'Partial\nContinuation Ratio',
			'Continuation Ratio',
			'Ordinal Model')))
perfData$value <- num(perfData$value)

# focus on somers d and viz
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
ggsave(somerViz, file=paste0(pathGraphics, 'fig_a_model_perf.pdf'),
       device=cairo_pdf, width=8, height=3)
###############################################################