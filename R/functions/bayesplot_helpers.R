####
library(bayesplot)
library(ggridges)

color_scheme_set("gray")

geom_ignore <- function(...){
  geom_blank(
    mapping = NULL, data = NULL,
    show.legend = FALSE, inherit.aes = FALSE) }

get_color <- function(levels) {
  sel <- which(!levels %in% scheme_level_names())
  if (length(sel))
    levels[sel] <- sapply(levels[sel], full_level_name)
  stopifnot(all(levels %in% scheme_level_names()))
  color_vals <- color_scheme_get()[levels]
  unlist(color_vals, use.names = FALSE) }

scheme_level_names <- function(){
  c("light",
    "light_highlight",
    "mid",
    "mid_highlight",
    "dark",
    "dark_highlight") }

geom_area_ridges <- function(...) {
  ggridges::geom_density_ridges(
    ..., stat = "identity", scale = .95) }

### modified version of bayesplot data building function
cleaner = function(x){gsub('\\.[0-9]\\.', '', x)}

prepData = function(gModBeta, typeLab){
	# stdz vars
	stdzCoef = function(coefVar, baseVar, dv){
		return( coefVar * (sd(baseVar)/sd(dv)) ) }
	vars = colnames(gModBeta)
	vars = vars[!grepl('Intercept',vars)]
	for(v in vars){
		gModBeta[,v] = stdzCoef(
			gModBeta[,v], 
			oppMod$data[,cleaner(v)],
			oppMod$data$icclevel_opp_3) }

	## prep data
	data = mcmc_areas_data(
	    gModBeta,
	    pars = colnames(gModBeta),
	    prob = 0.95, prob_outer=1,
	    point_est = 'mean'
	    ) 

	x = mcmc_areas_data(
		    gModBeta, pars = colnames(gModBeta),
		    prob = 0.9, prob_outer=1, 
		    point_est = 'mean'
	    ) %>%
		data.frame(.,stringsAsFactors = FALSE)
	x = x[x$interval=='inner',] ; x$interval = 'inner2'

	datas <- split(data, data$interval)
	datas$inner2 = x

	# add type
	datas = lapply(datas, function(x){x$type=typeLab;return(x)})

	#
	return(datas) }

### modified version of bayesplot viz function	
# colors for sig
coefp_colors = c(
    "Positive"=rgb(54, 144, 192, maxColorValue=255),
    "Negative"= rgb(222, 45, 38, maxColorValue=255),
    "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
    "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
    "Insignificant" = rgb(150, 150, 150, maxColorValue=255)
    )

# add sig col to beta df gen from getCIVecs
getSigVec = function(beta){
    beta$sig = NA
    beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
    beta$sig[beta$lo95 > 0] = "Positive"
    beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
    beta$sig[beta$hi95 < 0] = "Negative"
    beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
    return(beta) }