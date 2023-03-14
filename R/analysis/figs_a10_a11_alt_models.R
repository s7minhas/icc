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
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_fin.rda'))
oppMod <- mod
load(paste0(pathResults, 'sobState_model1a_1_newp5Var_fin.rda'))
stateMod <- mod
###############################################################

###############################################################
# vars
varsRaw <- unique(
	c( rownames(fixef(oppMod)), rownames(fixef(stateMod)) ) )
vars <- unique(gsub('\\[[1-9]\\]','',varsRaw))
varKey <- data.frame(
	dirty = vars, stringsAsFactors = FALSE )
varKey$clean <- c(
	'Intercept',
	'ICC\\,Ratification',
	'Civil War$_{t-1}$',
	'Polity$_{t-1}$',
	'Log(GDP per capita)$_{t-1}$',
	'African',
	'Judicial\n Independence$_{t-1}$',
	'Cumulative\n Opp OSV$_{t-1}$',
	'P5 Min. Ideal Pt.$_{t-1}$',
	'Cumulative\n Govt OSV$_{t-1}$'
	)
addCats <- function(x,toAdd){
	x$dirty=paste0(x$dirty,toAdd);x}
varKey <- rbind(varKey,
	addCats(varKey,'.1.'),addCats(varKey,'.2.') )
###############################################################    

###############################################################
# compare results with seq no cs and ordinal
mod = stateMod
data <- mod$data

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
baseOrd <- vglm(
	polrForm, data=data,
	family = cumulative (link="probitlink", parallel=TRUE))

# run Partial\nContinuation Ratio with no cs terms
baseSeq <- vglm(
	polrForm, data=data,
	family = sratio (link="logitlink", parallel=TRUE))

# extract fixed effs
coefOrd <- attributes(summaryvglm(baseOrd))$coef3[-(1:2),]
coefSeq <- attributes(summaryvglm(baseSeq))$coef3[-(1:2),]
coefSeq2 <- data.frame(fixef(mod)[-(1:2),],
	stringsAsFactors=FALSE)

# org
cleanCoef = function(modFixef, modLab){
	coefData = data.frame(modFixef[,1:2],
		stringsAsFactors=FALSE, row.names=NULL)
	coefData$var = rownames(modFixef)
	coefData$model = modLab
	return(coefData) }

coefData = rbind(
	cleanCoef(coefOrd, 'Ordinal Model'),
	cleanCoef(coefSeq, 'Continuation Ratio') )

attach(coefData)
coefData$Q2.5 = Estimate - qnorm(.975)*Std..Error
coefData$Q97.5 = Estimate + qnorm(.975)*Std..Error
detach(coefData)

coefData = coefData[,c('var','model','Estimate','Q2.5','Q97.5')]
coefSeq2$var = rownames(coefSeq2)
coefSeq2$var = gsub('[','.',coefSeq2$var,fixed=TRUE)
coefSeq2$var = gsub(']','.',coefSeq2$var,fixed=TRUE)
coefSeq2$model = 'Partial\nContinuation Ratio'
coefSeq2 = coefSeq2[,c('var','model','Estimate','Q2.5','Q97.5')]
coefData = rbind(coefData, coefSeq2)

# clean for plotting
# add var names from varKey
varKey2 = varKey[
	c(1,11,21,2,12,22,3,13,23,
		4,14,24,5,15,25,6,16,26,
		7,17,27,8,18,28,9,19,29,
		10,20,30
	), ]
varKey2$clean = gsub('$_{t-1}$', '', varKey2$clean, fixed=TRUE)
varKey2$clean2 = varKey2$clean

l1Keys = grepl('.1.',varKey2[,1], fixed=TRUE)
varKey2[l1Keys,'clean'] = paste0(
	varKey2[l1Keys,'clean'],
	'\n(No ICC to Prelim)')
varKey2[l1Keys,'clean2'] = '(No ICC to Prelim)      '

l2Keys = grepl('.2.',varKey2[,1], fixed=TRUE)
varKey2[l2Keys,'clean'] = paste0(
	varKey2[l2Keys,'clean'],
	'\n(Prelim to Formal)')
varKey2[l2Keys,'clean2'] = '(Prelim to Formal)      '

coefData$varClean = varKey2$clean[match(coefData$var, varKey2$dirty)]
coefData$varClean = factor(coefData$varClean,
	levels=rev(varKey2$clean))
coefData$var = factor(coefData$var,
	levels=rev(varKey2$dirty))
coefData$model <- factor(coefData$model,
		levels=rev(c(
			'Partial\nContinuation Ratio',
			'Continuation Ratio',
			'Ordinal Model')))

posDodge = .85
xlabels = varKey2$clean2
names(xlabels) = varKey2$dirty
xlabels=gsub('\n','',xlabels,fixed=TRUE)
xlabels=gsub('Independence', 'Indep', xlabels)
xlabels=gsub('Cumulative', 'Cumul', xlabels)

tmp_bw=ggplot(coefData, aes(x=var, y=Estimate, group=model)) +
  geom_hline(aes(yintercept=0), linetype=2, color = "black") +
  geom_point(aes(shape=model), size=4, position=position_dodge(width = posDodge)) +
  geom_errorbar(
		aes(ymin=Q2.5,ymax=Q97.5),
		linetype = 1,
		size = .5,
		width = 0.1,
		position=position_dodge(width = posDodge)) +
  scale_shape_manual(name = "", values = c(15, 18, 17)) +
  coord_flip() + labs(title = "", y="", x = "") +
	facet_wrap(~model, scales='free_x') +
	scale_x_discrete(labels=xlabels) +
	geom_vline(aes(xintercept=3.5), linetype='longdash') +
	geom_vline(aes(xintercept=6.5), linetype='longdash') +
	geom_vline(aes(xintercept=9.5), linetype='longdash') +
	geom_vline(aes(xintercept=12.5), linetype='longdash') +
  theme(
  	legend.position = 'none',
  	axis.text.x=element_text(size=12),
  	axis.text.y=element_text(size=14),
  	axis.ticks = element_blank(),
  	panel.border=element_blank(),
    strip.text.x = element_text(
			size = 14,
			color='white',
			angle=0, hjust=.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  	)
ggsave(tmp_bw,
	file=paste0(pathGraphics, 'fig_a10.pdf'),
	width = 12, height = 8)
###############################################################

###############################################################
# compare results with seq no cs and ordinal
mod = oppMod
data <- mod$data

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
baseOrd <- vglm(
	polrForm, data=data,
	family = cumulative (link="probitlink", parallel=TRUE))

# run Partial\nContinuation Ratio with no cs terms
baseSeq <- vglm(
	polrForm, data=data,
	family = sratio (link="logitlink", parallel=TRUE))

# extract fixed effs
coefOrd <- attributes(summaryvglm(baseOrd))$coef3[-(1:2),]
coefSeq <- attributes(summaryvglm(baseSeq))$coef3[-(1:2),]
coefSeq2 <- data.frame(fixef(mod)[-(1:2),],
	stringsAsFactors=FALSE)

# org
cleanCoef = function(modFixef, modLab){
	coefData = data.frame(modFixef[,1:2],
		stringsAsFactors=FALSE, row.names=NULL)
	coefData$var = rownames(modFixef)
	coefData$model = modLab
	return(coefData) }

coefData = rbind(
	cleanCoef(coefOrd, 'Ordinal Model'),
	cleanCoef(coefSeq, 'Continuation Ratio') )

attach(coefData)
coefData$Q2.5 = Estimate - qnorm(.975)*Std..Error
coefData$Q97.5 = Estimate + qnorm(.975)*Std..Error
detach(coefData)

coefData = coefData[,c('var','model','Estimate','Q2.5','Q97.5')]
coefSeq2$var = rownames(coefSeq2)
coefSeq2$var = gsub('[','.',coefSeq2$var,fixed=TRUE)
coefSeq2$var = gsub(']','.',coefSeq2$var,fixed=TRUE)
coefSeq2$model = 'Partial\nContinuation Ratio'
coefSeq2 = coefSeq2[,c('var','model','Estimate','Q2.5','Q97.5')]
coefData = rbind(coefData, coefSeq2)

# clean for plotting
# add var names from varKey
varKey2 = varKey[
	c(1,11,21,2,12,22,3,13,23,
		4,14,24,5,15,25,6,16,26,
		7,17,27,8,18,28,9,19,29,
		10,20,30
	), ]
varKey2$clean = gsub('$_{t-1}$', '', varKey2$clean, fixed=TRUE)
varKey2$clean2 = varKey2$clean

l1Keys = grepl('.1.',varKey2[,1], fixed=TRUE)
varKey2[l1Keys,'clean'] = paste0(
	varKey2[l1Keys,'clean'],
	'\n(No ICC to Prelim)')
varKey2[l1Keys,'clean2'] = '(No ICC to Prelim)      '

l2Keys = grepl('.2.',varKey2[,1], fixed=TRUE)
varKey2[l2Keys,'clean'] = paste0(
	varKey2[l2Keys,'clean'],
	'\n(Prelim to Formal)')
varKey2[l2Keys,'clean2'] = '(Prelim to Formal)      '

coefData$varClean = varKey2$clean[match(coefData$var, varKey2$dirty)]
coefData$varClean = factor(coefData$varClean,
	levels=rev(varKey2$clean))
coefData$var = factor(coefData$var,
	levels=rev(varKey2$dirty))
coefData$model <- factor(coefData$model,
		levels=rev(c(
			'Partial\nContinuation Ratio',
			'Continuation Ratio',
			'Ordinal Model')))

posDodge = .85
xlabels = varKey2$clean2
names(xlabels) = varKey2$dirty
xlabels=gsub('\n','',xlabels,fixed=TRUE)
xlabels=gsub('Independence', 'Indep', xlabels)
xlabels=gsub('Cumulative', 'Cumul', xlabels)

tmp_bw=ggplot(coefData, aes(x=var, y=Estimate, group=model)) +
  geom_hline(aes(yintercept=0), linetype=2, color = "black") +
  geom_point(aes(shape=model), size=4, position=position_dodge(width = posDodge)) +
  geom_errorbar(
		aes(ymin=Q2.5,ymax=Q97.5),
		linetype = 1,
		size = .5,
		width = 0.1,
		position=position_dodge(width = posDodge)) +
  scale_shape_manual(name = "", values = c(15, 18, 17)) +
  coord_flip() + labs(title = "", y="", x = "") +
	facet_wrap(~model, scales='free_x') +
	scale_x_discrete(labels=xlabels) +
	geom_vline(aes(xintercept=3.5), linetype='longdash') +
	geom_vline(aes(xintercept=6.5), linetype='longdash') +
	geom_vline(aes(xintercept=9.5), linetype='longdash') +
	geom_vline(aes(xintercept=12.5), linetype='longdash') +
  theme(
  	legend.position = 'none',
  	axis.text.x=element_text(size=12),
  	axis.text.y=element_text(size=14),
  	axis.ticks = element_blank(),
  	panel.border=element_blank(),
    strip.text.x = element_text(
			size = 14,
			color='white',
			angle=0, hjust=.05),
    strip.background = element_rect(fill = "#525252", color='#525252')
  	)
ggsave(tmp_bw,
	file=paste0(pathGraphics, 'fig_a11.pdf'),
	width = 12, height = 8)
###############################################################