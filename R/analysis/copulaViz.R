###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('herme','Owner', 'S7M')){
	user <- Sys.info()['user']
	baseDir <- paste0('C:/Users/',Sys.info()['user'],'/')
	source( paste0(baseDir,'Research/icc/R/setup.R') )
	pathGraphics = paste0(baseDir, 'Research/icc/iccPaper/')
}

#
set.seed(6886)
loadPkg(
	c(
		'sbgcop'
		)
	)

load(paste0(pathData, 'sobOpp_imp.rda'))

names(impData)
