####
library(bayesplot)
library(ggridges)
library(gridExtra)
library(Cairo)
library(latex2exp)

### modified version of bayesplot data building function
# add color column for coef dist plot
addSomeColor = function(rawBeta, dataList){

	# add sig col to beta df gen from getCIVecs
	getSigVec = function(beta){
	    beta$sig = NA
	    beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
	    beta$sig[beta$lo95 > 0] = "Positive"
	    beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
	    beta$sig[beta$hi95 < 0] = "Negative"
	    beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
	    return(beta) }

	# add coefp_colors
	sigSumm = apply(rawBeta, 2, function(x){
	    qt95 = quantile(x, probs=c(0.025, 0.975))
	    qt90 = quantile(x, probs=c(0.05, 0.95))
	    c(qt95, qt90) })
	rownames(sigSumm) = c('lo95', 'hi95','lo90','hi90')
	sigSumm = data.frame(t(sigSumm), stringsAsFactors = FALSE)
	sigSumm$var = rownames(sigSumm) ; rownames(sigSumm) = NULL
	sigSumm = getSigVec(sigSumm)

	#
	dataList = lapply(dataList, function(x){
	    x$sig = sigSumm$sig[match(char(x$parameter), sigSumm$var)]
	    return(x) })
	return(dataList) }

# prep data into format for coef dist plot
prepData = function(gModBeta, stanModel, typeLab){

	# remove category-specific labels
	cleaner = function(x){gsub('\\.[0-9]\\.', '', x)}

	# stdz vars
	stdzCoef = function(coefVar, baseVar, dv){
		stdzVar = coefVar * (sd(baseVar)/sd(dv))
		return( stdzVar ) }

	# scale vars for plotting
	vars = colnames(gModBeta)
	vars = vars[!grepl('Intercept',vars)]
	for(v in vars){
		gModBeta[,v] = stdzCoef(
			gModBeta[,v], 
			stanModel$data[,cleaner(v)],
			stanModel$data[,1]) }

	## prep data
	data = mcmc_areas_data(
	    gModBeta,
	    pars = colnames(gModBeta),
	    prob = 0.95, prob_outer=1,
	    point_est = 'mean'
	    ) 
	dataList <- split(data, data$interval)

	# add one more inner dist 
	x = mcmc_areas_data(
		    gModBeta, pars = colnames(gModBeta),
		    prob = 0.9, prob_outer=1, 
		    point_est = 'median'
	    ) %>%
		data.frame(.,stringsAsFactors = FALSE)
	x = x[x$interval=='inner',] ; x$interval = 'inner2'
	dataList$inner2 = x

	# add type
	dataList = lapply(dataList, function(x){x$type=typeLab;return(x)})

	# add some color
	dataList = addSomeColor(gModBeta, dataList)

	#
	return(dataList) }

### modified version of bayesplot viz function	
# colors for sig
coefp_colors = c(
    "Positive"=rgb(54, 144, 192, maxColorValue=255),
    "Negative"= rgb(222, 45, 38, maxColorValue=255),
    "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
    "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
    "Insignificant" = rgb(150, 150, 150, maxColorValue=255)
    )

# revised fn for final viz
mcmcViz = function(dataList, varLabels, colorsForCoef=coefp_colors){

	# fn from bayesplot needs this
	geom_area_ridges <- function(...) {
	  ggridges::geom_density_ridges(
	    ..., stat = "identity", scale = .95) }

	# dist ranges
	x_lim <- range(dataList$outer$x)
	x_range <- diff(x_lim)
	x_lim[1] <- x_lim[1] - 0.05 * x_range
	x_lim[2] <- x_lim[2] + 0.05 * x_range

	# args for layers
	args_inner <- list(
	    mapping = aes_(height = ~density, color=~sig, fill=~sig), 
	    alpha=.3, data = dataList$inner)
	args_inner2 <- list(
	    mapping = aes_(height = ~density, color=~sig, fill=~sig), 
	    alpha=.4, data = dataList$inner2)
	args_point <- list(
	    mapping = aes_(height = ~density, color=~sig, fill=~sig), 
	    data = dataList$point, color = NA)
	args_outer <- list(
	    mapping = aes_(height = ~density, color=~sig), alpha=.6, fill = NA)

	# wrap into layers
	layer_inner <- do.call(geom_area_ridges, args_inner)
	layer_inner2 <- do.call(geom_area_ridges, args_inner2)
	layer_point <- do.call(geom_area_ridges, args_point)
	layer_outer <- do.call(geom_area_ridges, args_outer)

	# viz
	gg = ggplot(dataList$outer) + 
	    aes_(x = ~x, y = ~parameter, color=~sig, fill=~sig) + 
	    layer_inner + 
	    layer_inner2 + 
	    layer_point + 
	    layer_outer + 
	    scale_color_manual(values=colorsForCoef, guide=FALSE) +
	    scale_fill_manual(values=colorsForCoef, guide=FALSE) +
	    scale_y_discrete(
	        limits = unique(rev(dataList[[1]]$parameter)), 
	        expand = c(0.05, 0.6) ) +
	    xlim(x_lim) +
	    bayesplot_theme_get() + 
	    legend_move("none") +
	    yaxis_text(face = "bold") + 
	    yaxis_title(FALSE) +
	    yaxis_ticks(size = 1) +
	    xaxis_title(FALSE) +
	    geom_vline(
	        aes(xintercept=0), 
	        color='grey60', linetype='dashed'
	        ) +
	    scale_y_discrete('', labels=TeX(varLabels)) +
	    theme_light(base_family="Source Sans Pro") +
	    theme_bw() +
	    facet_wrap(~type, scales='free_x') +
	    theme(
	        axis.ticks=element_blank(),
	        panel.border=element_blank(),
			axis.text.x=element_text(family="Source Sans Pro Light"),
			axis.text.y=element_text(family="Source Sans Pro Light", hjust=0),
	        strip.text.x = element_text(size = 9, color='white',
	            family="Source Sans Pro Semibold", 
	            angle=0, hjust=.05),
	        strip.background = element_rect(fill = "#525252", color='#525252')        
	        )	
	   #
	   return(gg) }

# wrapper function around various stages necessary to process model
vizWrapper = function(model, gLab, l1Lab, l2Lab){
	# viz
	betaMatrix = data.frame(
		fixef(model, summary=FALSE),
		stringsAsFactors = FALSE
		)

	# org vars
	gVars = colnames(betaMatrix)[
		!grepl('.',colnames(betaMatrix), fixed=TRUE) ]
	l1Vars = colnames(betaMatrix)[
		grepl('.1.',colnames(betaMatrix), fixed=TRUE) ]
	l2Vars = colnames(betaMatrix)[
		grepl('.2.',colnames(betaMatrix), fixed=TRUE) ]

	# viz
	varLabs = varKey$clean
	names(varLabs) = varKey$dirty
	ggGlobal = mcmcViz(
		prepData(betaMatrix[,gVars], model, gLab), varLabs)
	ggLevel1 = mcmcViz(
		prepData(betaMatrix[,l1Vars[-1]], model, l1Lab), varLabs)
	ggLevel2 = mcmcViz(
		prepData(betaMatrix[,l2Vars[-1]], model, l2Lab), varLabs) +
		theme(
			axis.text.y = element_blank()
			)

	# arrange viz
	## helper for arranging plot
	return( list(g=ggGlobal,l1=ggLevel1,l2=ggLevel2) ) }
