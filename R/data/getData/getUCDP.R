####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

############################
# battle deaths conflict level data
load(paste0(pathData, 'ucdp/ucdp-brd-conf-172.rda'))
############################

############################
# define civil wars at country year level
civWar = ucdp.brd[
	ucdp.brd$type_of_conflict %in% 3:4,
	c('year','gwno_loc')] %>%
	unique()

civWar$cname = panel$cname[match(civWar$gwno_loc,panel$ccode)]
############################

### regen osv variables from one sided violence data

### 