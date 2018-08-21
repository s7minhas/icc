if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

ec2Path = paste0(pathGit, 'from_ec2/for_ec2/')

loadPkg('bartMachine')
load(paste0(pathData, 'mergedData.rda'))

# load files
toload=list.files(paste0(ec2Path, 'results'))
toload=paste0(ec2Path,'results/',toload)
invisible(lapply(toload,load,.GlobalEnv))

#
summary(bm_prelimOpp)
plot_convergence_diagnostics(bm_prelimOpp)