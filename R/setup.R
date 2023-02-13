rm(list=ls())

if(Sys.info()['user'] %in% c('herme','Owner','S7M')){
  user = Sys.info()['user']
	baseDir = paste0('C:/Users/',user,'/')
  pathGit=paste0(baseDir, 'Research/icc/')
  pathDrop=paste0(baseDir, 'Dropbox/Research/icc/')
  pathData=paste0(pathDrop, 'data/')
  pathResults=paste0(pathDrop,'results/')
  pathGraphics=paste0(pathDrop,'graphics/') }

if(Sys.info()['user'] %in% c('s7m', 'janus829', 'sminhas')){
	pathGit='~/Research/icc/'
	pathDrop='~/Dropbox/Research/icc/'
  # pathDrop='/Volumes/Samsung_X5/Dropbox/Research/icc/'
	pathData=paste0(pathDrop, 'data/')
	pathResults=paste0(pathDrop,'results/')
	pathGraphics=paste0(pathDrop,'graphics/') }

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
