rm(list=ls())
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	pathDrop='~/Dropbox/Research/icc/'
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
	'dplyr', 'reshape2', 'magrittr', 'lubridate',
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

# helper dataset for matching country names
load(paste0(pathData, 'panel.rda'))