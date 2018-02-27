if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

# load in AP data and keep only their key vars
# will rebuild rest in subseq files
orig  = read_dta(
	paste0(pathData, 'icc_from_ap/iccdata_013018.dta')
	)

vars = c(
	'icclevel',
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

# create new df and get rid of tibble formatting
apData = data.frame(orig[,c(ids, vars)], stringsAsFactors=FALSE)

# add in stdz cnames
apData$cname = cname(apData$statenme)

# reorg
apData$d = '01'
apData$m = ifelse(
	nchar(apData$month)>1, 
	apData$month, paste0('0',apData$month) )
apData$date = ymd(
	paste(apData$year,apData$m,apData$d, sep='-') )
apData$cdate = paste(apData$cname, apData$date, sep='_')
apData = apData[,c('cname','cdate','date',ids,vars)]

# subset to when dv available
apData = apData[
	which(
		apData$date > as.Date('2002-06-01', format='%Y-%m-%d'))
	,]

# save and move onto merging
save(apData, file=paste0(pathData, 'apDataRaw.rda'))