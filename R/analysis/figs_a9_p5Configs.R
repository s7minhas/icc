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
cntries = c('CHN', 'FRA', 'RUS', 'UK', 'USA')
oFiles = paste0( pathResults, 'sobOpp_p_', cntries, '_fin.rda' )
sFiles = paste0( pathResults, 'sobState_p_', cntries, '_fin.rda' )
oppMods = lapply(oFiles, function(x){load(x);mod })
stateMods = lapply(sFiles, function(x){load(x);mod })
names(oppMods) = names(stateMods) = cntries
###############################################################

###############################################################
# vars
varsRaw = unique(
	c( rownames(fixef(oppMods[[1]])), rownames(fixef(stateMods[[1]])) ) )
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
	'Ideal Point\n Distance w/ Country',
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
# get summary of coef vals
oppCoef = do.call('rbind', lapply(cntries, function(cntry){
    coef = stanSumm(oppMods[[cntry]], 'Opposition-Focused')
    coef$p5cntry = cntry
    return(coef) }) )
stateCoef = do.call('rbind', lapply(cntries, function(cntry){
    coef = stanSumm(stateMods[[cntry]], 'State-Focused')
    coef$p5cntry = cntry
    return(coef) }) )

# organize
coefData = rbind(stateCoef, oppCoef)

# reorder vars
coefData$stage = factor( coefData$stage, levels=c(
    'Global Effects',
    'No ICC to\nPrelim Effects',
    'ICC Prelim to\nFormal Effects' ))
coefData$clean = factor(
    coefData$clean, levels=unique(varKey$clean[-1]))
coefData$type = factor(
    coefData$type, levels=c('State-Focused', 'Opposition-Focused'))
###############################################################

###############################################################
# make coef plot
viz=ggplot(
	data=coefData, 
	aes(x=clean, y=Estimate, color=p5cntry)) +
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
###############################################################

###############################################################
# save
ggsave(viz,
	file=paste0(pathGraphics, 'fig_a9.pdf'),
	width=8, height=8, device=cairo_pdf)
###############################################################
