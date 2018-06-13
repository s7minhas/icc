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
	'poi_osv_total', 'poi_pts',
	'civilwar',
	'icc_stage1',
	'icc_stage2',
	'icc_onset',
	'icc_rat'
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
		icc_stage1 = ifelse(any(icc_stage1>=1), 1, 0),
		icc_stage2 = ifelse(any(icc_stage2>=1), 1, 0),
		icc_onset = ifelse(any(icc_onset>=1), 1, 0),
		poi_osv_state = sum(poi_osv_state, na.rm=TRUE),
		poi_osv_rebel = sum(poi_osv_rebel, na.rm=TRUE),
		poi_osv_total = sum(poi_osv_total, na.rm=TRUE),
		poi_pts = mean(poi_pts, na.rm=TRUE),
		civilwar = ifelse(any(civilwar>0), 1, 0),
		icc_rat = ifelse(any(icc_rat>0), 1, 0),		
		) %>% data.frame()

# add year level ids
data$cnameYear = with(data, paste(cname, year, sep='_'))
data$ccodeYear = with(data, paste(ccode, year, sep='_'))
###############################################################

###############################################################
# add lagged versions of poi and civilwar var
tmp = data; tmp$year = tmp$year + 1
tmp$ccodeYear = with(tmp, paste(ccode, year, sep='_'))
vars = names(data)[13:17]

for(v in vars){
	data$tmp = tmp[match(data$ccodeYear, tmp$ccodeYear),v]
	names(data)[ncol(data)] = paste0('lag1_', v) }

# remove original non-lagged versions
data = data[,-match(vars, names(data))]
###############################################################

###############################################################
# small fixes
data$lag1_civilwar[is.na(data$lag1_civilwar)] = 0
data$lag1_poi_pts[is.nan(data$lag1_poi_pts)] = NA

vars = paste0('lag1_poi_osv_',c('state','total','rebel'))
for(v in vars){data[is.na(data$poi_pts),v] = NA}
###############################################################

###############################################################
# reorder columns
data = data[,c(1:3,14:15,4:13,16:20)]

# save and move onto merging
save(data, file=paste0(pathData, 'baseData.rda'))
###############################################################