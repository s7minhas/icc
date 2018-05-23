# ####
# if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
# 	source('~/Research/icc/R/setup.R') }
# ####

############################
# battle deaths conflict level data
load(paste0(pathData, 'ucdp/ucdp-onesided-172.rda'))
############################

############################
# define civil wars at country year level
head(slice)
tocompare = unique(slice[,c('statenme','year','month','osv_state','osv_rebel','osv_total')])

fckme = ucdp.osv[grepl('Sudan', ucdp.osv$location),]
fckme2 = fckme[fckme$is_government_actor==0,]
aggstats = fckme2 %>%
	group_by(year) %>% 
	summarize(
		best_deaths = sum(best_fatality_estimate),
		low_deaths = sum(low_fatality_estimate),
		high_deaths = sum(high_fatality_estimate)
		)


load(paste0(pathData, 'ucdp/ged171.Rdata'))

fckme = ged171[ged171$country=='Sudan',]
fckme = fckme[fckme$year>=2005,]
fckme = fckme[,c('')]
############################

### regen osv variables from one sided violence data

### 