###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################

###############################################################
# load in AP data and keep only their key vars
# will rebuild rest in subseq files
orig  = read_dta(
	paste0(pathData, 'icc_from_ap/iccdata_013018.dta')
	)

vars = c(
	'icclevel',
	'icclevel_state',
	'icclevel_opp',
	'poi_osv_state', 'poi_osv_rebel',
	'poi_osv_total', 'poi_pts'
	)

ids = c(
	'ccode',
	'statenme',
	'year',
	'month'
	)

# create new df and get rid of tibble formatting
apData = data.frame(orig[,c(ids, vars)], stringsAsFactors=FALSE)
###############################################################

###############################################################
# add in stdz cnames
apData$statenme[apData$statenme=='Yugoslavia'] = 'SERBIA'
apData$cname = cname(apData$statenme)
apData$ccode = panel$ccode[match(apData$cname, panel$cname)]
###############################################################

###############################################################
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
		apData$date < as.Date('2016-12-01', format='%Y-%m-%d')
		)
	,]
###############################################################

###############################################################
# aggregate to year level
data = apData %>% 
	dplyr::group_by(cname, ccode, year) %>%
	dplyr::summarize(
		prelim_icc = ifelse(any(icclevel==1), 1, 0),
		formal_icc = ifelse(any(icclevel>1), 1, 0),
		prelim_icc_state = ifelse(any(icclevel_state==1), 1, 0),
		formal_icc_state = ifelse(any(icclevel_state>1), 1, 0),
		prelim_icc_opp = ifelse(any(icclevel_opp==1), 1, 0),
		formal_icc_opp = ifelse(any(icclevel_opp>1), 1, 0),
		poi_osv_state = sum(poi_osv_state, na.rm=TRUE),
		poi_osv_rebel = sum(poi_osv_rebel, na.rm=TRUE),
		poi_osv_total = sum(poi_osv_total, na.rm=TRUE),
		poi_pts = mean(poi_pts, na.rm=TRUE)
		) %>% data.frame()

# add year level ids
data$cnameYear = with(data, paste(cname, year, sep='_'))
data$ccodeYear = with(data, paste(ccode, year, sep='_'))
###############################################################

###############################################################
# save and move onto merging
save(data, file=paste0(pathData, 'baseData.rda'))
###############################################################