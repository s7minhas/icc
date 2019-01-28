###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################

# get states that experienced a civil war or had a pts >=3
if(!file.exists(paste0(pathData, 'subset_ptsCivWar_cntries.rda'))){
cntries = frame %>% group_by(ccode) %>% summarize(
	civilWar = max(lag1_civilwar),
	pts = max(lag1_pts)
	) %>% filter(civilWar == 1 | pts >=3) %>% data.frame()
save(cntries, file=paste0(pathData, 'subset_ptsCivWar_cntries.rda'))
	} else { load(paste0(pathData, 'subset_ptsCivWar_cntries.rda')) }

cntries2 = data %>% group_by(ccode) %>% summarize(
	civilWar = max(lag1_civilwar, na.rm=TRUE),
	pts = max(lag1_pts, na.rm=TRUE)	
	) %>% filter(civilWar == 1 | pts >=3) %>% data.frame()