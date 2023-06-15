# clear objs
rm(list=ls())

# set git paths
require(here)
pth = paste0(here::here(), '/')

# set paths to drop folders
pathIn = pathData = paste0(pth, 'data/')
pathOut = pathResults = paste0(pth, 'results/')
pathGraphics = paste0(pth, 'graphics/')
pathFuncs = paste0(pth, 'functions/')

#' function to install and/or load packages
#'
#' @param toLoad character vector of packages to load
#' @author Shahryar Minhas
#' @return adds libraries to worksapce
#' @export
#'
loadPkg=function(toLoad){
	for(lib in toLoad){
	  if(!(lib %in% installed.packages()[,1])){
	    install.packages(lib, repos='http://cran.rstudio.com/') }
	  suppressMessages( library(lib, character.only=TRUE) )
	}
}

# some necessary libs
loadPkg(c(
	'foreign', 'haven',
	'dplyr', 'reshape2', 'magrittr', 'lubridate', 'tidyr',
	'ggplot2', 'latex2exp', 'Cairo',	# plotting
	'foreach', 'doParallel', # parallelization
	'xtable', # tables
	'devtools', # loading git packages
	'countrycode' # v 0.16
	))
  # other packages used in scripts
  # 'sbgcop', 'brms', 'future', 'bayesplot', 'MASS', 'VGAM',
  # 'tadaatoolbox', 'reshape2', 'RColorBrewer',
  # 'extrafont', 'latex2exp', 'Cairo', 'gridExtra', 'cowplot'
  # 'sf','dplyr',
  # 'rnaturalearth',
  # 'rgeos', 'hrbrthemes'

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
pasteMult = function(x,y,sepZ){
	apply(expand.grid(x,y), 1, paste, collapse=sepZ) }
cname = function(x) {countrycode(x,'country.name','country.name')}

# helper to calculate p5 vars
p5Vars = function(df, vars, baseLabel){

	# sum up
	df$tmp = apply(df[,vars],1,sum,na.rm=TRUE)
	names(df)[ncol(df)] = paste0('p5_',baseLabel)

	# calc avg
	df$tmp = apply(df[,vars], 1, mean, na.rm=TRUE)
	names(df)[ncol(df)] = paste0('p5_',baseLabel,'Avg')

	# calc min/max
	df$tmp = apply(df[,vars],1, min,na.rm=TRUE)
	names(df)[ncol(df)] = paste0('p5_',baseLabel,'Min')
	df$tmp = apply(df[,vars],1, max,na.rm=TRUE)
	names(df)[ncol(df)] = paste0('p5_',baseLabel,'Max')

	#
	return(df) }

# rubin coef
rubinCoef = function (coefMatrix, seMatrix){
    numMods = nrow(coefMatrix)
    ones = matrix(1, nrow = 1, ncol = numMods)
    coefMelt = (ones %*% coefMatrix)/numMods
    se2 = (ones %*% (seMatrix^2))/numMods
    diff = coefMatrix - matrix(1, nrow = numMods, ncol = 1) %*% coefMelt
    sq2 = (ones %*% (diff^2))/(numMods - 1)
    seMelt = sqrt(se2 + sq2 * (1 + 1/numMods))
    return(list(beta = coefMelt, se = seMelt)) }

# quick table summ
getTable = function(coefs, vnames, modelSumm, modelNames, digs=2){
  noModels=length(modelSumm)
  tableResults = matrix('', nrow=2*length(coefs), ncol=1+noModels)
  tableResults[,1] = rep(coefs,2)
  colnames(tableResults) = c('Variable',paste('Model',1:noModels))
  for(ii in 2:ncol(tableResults)){
    temp = modelSumm[[ii-1]]
    temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
    estims = temp[1:length(coefs),'beta']
    estims = round(as.numeric(as.character(estims)),digs)
    tvals = abs(temp[1:length(coefs),'z'])
    tvals = round(as.numeric(as.character(tvals)),digs)
    estims = ifelse(tvals>=qnorm(0.95) & !is.na(tvals) & tvals<qnorm(0.975),
      paste('$', estims,'^{\\ast}$',sep=''), estims)
    estims = ifelse(tvals>=qnorm(0.975) & !is.na(tvals),
      paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)
    tableResults[1:length(coefs),ii] = estims
    serrors = temp[(length(coefs)+1):nrow(tableResults),'serror']
    serrors = round(as.numeric(as.character(serrors)),digs)
    serrors = paste('(',serrors,')',sep='')
    serrors = ifelse(serrors=='(NA)','',serrors)
    tableResults[(length(coefs)+1):nrow(tableResults),ii] = serrors
  }

  # Reorganizing rows and variable labels
  tableFinal = NULL
  for(ii in 1:length(coefs)){
    temp = cbind('', t(tableResults[ii+length(coefs),2:ncol(tableResults)]))
    tableFinal = rbind(tableFinal, tableResults[ii,], temp) }
  tableFinal[,'Variable'] = vnames[match(tableFinal[,'Variable'],coefs)]
  tableFinal[,'Variable'][is.na(tableFinal[,'Variable'])] = ''

  tableFinal = rbind(tableFinal)
  colnames(tableFinal)[2:(noModels+1)] = modelNames
  return(tableFinal) }

# helper dataset for matching country names
load(paste0(pathData, 'panel.rda'))


#
coefp_colors = c(
  "Positive"=rgb(54, 144, 192, maxColorValue=255),
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255)
)

#
coefp_colors_grey = coefp_colors
coefp_colors_grey[1:2] = 'grey30'
coefp_colors_grey[3:4] = 'grey50'


##################
# Sys and package info
# > sessionInfo()
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Pop!_OS 22.04 LTS

# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

# locale:
#  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

# time zone: America/Detroit
# tzcode source: system (glibc)

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# loaded via a namespace (and not attached):
# [1] compiler_4.3.0 cli_3.6.1      jsonlite_1.8.5 rlang_1.1.1 

# > benchmarkme::get_cpu()
# $vendor_id
# [1] "AuthenticAMD"

# $model_name
# [1] "AMD Ryzen 9 7950X 16-Core Processor"

# $no_of_cores
# [1] 32

# > benchmarkme::get_ram()
# 134 GB

# # libs
# pkgs = c(
# 	'foreign', 'haven',
# 	'dplyr', 'reshape2', 'magrittr', 'lubridate', 'tidyr',
# 	'ggplot2', 'latex2exp', 'Cairo',	# plotting
# 	'foreach', 'doParallel', # parallelization
# 	'xtable', # tables
# 	'devtools', # loading git packages
# 	'countrycode', # v 0.16
#   'sbgcop', 'brms', 'future', 'bayesplot', 'MASS', 'VGAM',
#   'tadaatoolbox', 'reshape2', 'RColorBrewer',
#   'extrafont', 'latex2exp', 'Cairo', 'gridExtra', 'cowplot',
#   'sf','dplyr',
#   'rnaturalearth',
#   'rgeos', 'hrbrthemes'  
# 	)
# pkgs = sort(unique(pkgs))
# vinfo = installed.packages()[pkgs, c('Package', 'Version')]
# rownames(vinfo) = NULL
# vinfo = apply(vinfo, 1, paste, collapse=' ')
# vinfo = matrix(vinfo, ncol=5)
# knitr::kable(vinfo, format='markdown')
# |                 |                  |                 |                    |                    |
# |:----------------|:-----------------|:----------------|:-------------------|:-------------------|
# |bayesplot 1.10.0 |doParallel 1.0.17 |ggplot2 3.4.2    |magrittr 2.0.3      |sbgcop 0.980        |
# |brms 2.19.0      |dplyr 1.1.2       |gridExtra 2.3    |MASS 7.3-59         |sf 1.0-13           |
# |Cairo 1.6-0      |extrafont 0.19    |haven 2.5.2      |RColorBrewer 1.1-3  |tadaatoolbox 0.17.0 |
# |countrycode 0.16 |foreach 1.5.2     |hrbrthemes 0.8.0 |reshape2 1.4.4      |tidyr 1.3.0         |
# |cowplot 1.1.1    |foreign 0.8-82    |latex2exp 0.9.6  |rgeos 0.6-3         |VGAM 1.1-8          |
# |devtools 2.4.5   |future 1.32.0     |lubridate 1.9.2  |rnaturalearth 0.3.3 |xtable 1.8-4        |

