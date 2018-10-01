###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################	

###############################################################
# Download file from A&P
setwd(paste0(pathData, 'apIntervention/'))
intv = read.csv('monthly_p5intervention_data.csv')
###############################################################

###############################################################
# Match ally names to panel
intv$cname = cname(intv$statenme)
intv$cname[intv$cname=='Yugoslavia'] = 'SERBIA'
intv$ccode = panel$ccode[match(intv$cname, panel$cname)]
###############################################################

###############################################################
# agg to year
intv = intv[,c(1,3,which(grepl('p5',names(intv))))]
intv = intv %>% group_by(ccode,year) %>%
	summarise_all(max) %>% data.frame()
###############################################################

###############################################################
# save
save(intv, file=paste0(pathData, 'apIntervention/intv.rda'))
###############################################################