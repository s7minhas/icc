###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################

###############################################################
# load data
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
###############################################################	

###############################################################
# get states that experienced a civil war or had a pts >=3
cntries = data %>% group_by(ccode) %>% summarize(
	civilWar = max(lag1_civilwar, na.rm=TRUE),
	pts = max(lag1_pts, na.rm=TRUE)	
	) %>% filter(civilWar == 1 | pts >=3) %>% data.frame()
###############################################################

###############################################################
save(cntries, file=paste0(pathData, 'subset_ptsCivWar_cntries.rda'))
###############################################################