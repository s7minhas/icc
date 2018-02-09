if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

# load in AP data and keep only their key vars
# will rebuild rest in subseq files
orig  = read_dta(paste0(pathData, 'icc_from_ap/iccdata_013018.dta'))

vars = c(
	'icclevel', #
	'icclevel_state',
	'icclevel_opp',
	'pts',
	'pts_lag',
	'poi_pts',
	'osv_state',
	'runsum_osvstate',
	'osv_rebel',
	'runsum_osvrebel',
	'osv_total',
	'runsum_osvtotal',
	'poi_osv_state',
	'poi_osv_rebel',
	'poi_osv_total',
	'civilwar',
	'intensitylevel',
	'civilwar_lag',
	'intensitylevel_lag',
	'runmax_civilwar',
	'runmax_intensitylevel'
	)

ids = c(
	'ccode',
	'statenme',
	'year',
	'month'
	)

#
apData = data.frame(orig[,c(ids, vars)], stringsAsFactors=FALSE)

# add in stdz cnames
apData$cname = cname(apData$statenme)

# reorg
apData$d = '01'
apData$m = ifelse(nchar(apData$month)>1, apData$month, paste0('0',apData$month))
apData$date = ymd(paste(apData$year,apData$m,apData$d, sep='-'))
apData$cdate = paste(apData$cname, apData$date, sep='_')
apData = apData[,c('cname','cdate','date',ids,vars)]

# create separate icc levels
for(v in 0:6){
	apData$tmp = ifelse(apData$icclevel==v, 1, 0)
	names(apData)[ncol(apData)] = paste0('icclevel',v)
}


# ok so data for DV starts after 6/2002
tmp = apData[
	apData$date > as.Date('2002-06-01', format='%Y-%m-%d'),
	c('cname',ids, 'date', 'icclevel',paste0('icclevel',0:6))
	]

tmp %>% group_by(year) %>%
	summarize(
		sum0 = sum(icclevel0),
		sum1 = sum(icclevel1),
		sum2 = sum(icclevel2),
		sum3 = sum(icclevel3),
		sum4 = sum(icclevel4),
		sum5 = sum(icclevel5),
		sum6 = sum(icclevel6)
		)

unique(tmp$cname[tmp$icclevel==6])