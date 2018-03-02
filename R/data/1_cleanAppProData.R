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

# subset to temporal frame
# if based on dv we'd go through 2016
# however because of if restrictions we go 
# to 12/2015
apData = apData[
	which(
		apData$date > as.Date('2002-06-01', format='%Y-%m-%d') &
		apData$date < as.Date('2016-01-01', format='%Y-%m-%d')
		)
	,]

# aggregate to year level
apDataYr = apData %>% 
	group_by(cname, ccode, year) %>%
	summarize(
		prelim_icc = ifelse(any(icclevel==1), 1, 0),
		formal_icc = ifelse(any(icclevel>1), 1, 0),
		prelim_icc_state = ifelse(any(icclevel_state==1), 1, 0),
		formal_icc_state = ifelse(any(icclevel_state>1), 1, 0),
		prelim_icc_opp = ifelse(any(icclevel_opp==1), 1, 0),
		formal_icc_opp = ifelse(any(icclevel_opp>1), 1, 0),
		pts = max(pts, na.rm=TRUE), 
		poi_pts = max(poi_pts, na.rm=TRUE),
		osv_state = sum(osv_state, na.rm=TRUE),
		runsum_osvstate = sum(runsum_osvstate, na.rm=TRUE),
		osv_rebel = sum(osv_rebel, na.rm=TRUE),
		runsum_osvrebel = sum(runsum_osvrebel, na.rm=TRUE),		
		osv_total = sum(osv_total, na.rm=TRUE),
		runsum_osvtotal = sum(runsum_osvtotal, na.rm=TRUE),				
		poi_osv_state = max(poi_osv_state, na.rm=TRUE),
		poi_osv_rebel = max(poi_osv_rebel, na.rm=TRUE),
		poi_osv_total = max(poi_osv_total, na.rm=TRUE),
		civilwar = max(civilwar, na.rm=TRUE),
		intensitylevel = max(intensitylevel, na.rm=TRUE),
		runmax_civilwar = max(runmax_civilwar, na.rm=TRUE),
		runmax_intensitylevel = max(runmax_intensitylevel, na.rm=TRUE)
		) %>% data.frame()

nrow(unique(apData[,c('cname','year','civilwar')])) ; nrow(unique(apData[,c('cname','year')])) ; nrow(apData) ; 

# save and move onto merging
save(apData, file=paste0(pathData, 'apDataRaw.rda'))