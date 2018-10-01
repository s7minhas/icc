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
	dplyr::mutate(
		prelim_icc = ifelse(icclevel==1, 1, 0),
		formal_icc = ifelse(icclevel>1, 1, 0),
		prelim_icc_state = ifelse(icclevel_state==1, 1, 0),
		formal_icc_state = ifelse(icclevel_state>1, 1, 0),
		prelim_icc_opp = ifelse(icclevel_opp==1, 1, 0),
		formal_icc_opp = ifelse(icclevel_opp>1, 1, 0)
		) %>% data.frame()

# add year level ids
data$cnameYear = with(data, paste(cname, year, sep='_'))
data$ccodeYear = with(data, paste(ccode, year, sep='_'))
###############################################################

###############################################################
# add lagged versions of poi and civilwar var
tmp = data; tmp$date = tmp$date %m+% months(1)
tmp$cdate = with(tmp, paste(cname, date, sep='_'))
vars = names(data)[11:15]

for(v in vars){
	data$tmp = tmp[match(data$cdate, tmp$cdate),v]
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
# fix sudan
data$prelim_icc_state[data$cname=="SUDAN" & data$year==2005] = 0
data$prelim_icc_opp[data$cname=="SUDAN" & data$year==2005] = 0
data$prelim_icc[data$cname=="SUDAN" & data$year==2005] = 0

# fix libya
data$prelim_icc_state[data$cname=="LIBYAN ARAB JAMAHIRIYA" & data$year==2011] = 0
data$prelim_icc[data$cname=="LIBYAN ARAB JAMAHIRIYA" & data$year==2011] = 0

# organize by state-year
data = data[order(data$ccodeYear),]

# save and move onto merging
save(data, file=paste0(pathData, 'baseData_mthly.rda'))
###############################################################

###############################################################

### dealing with formal pool ... 
# 1. go back down to the monthly level ... only include cases that have experienced prelims
# 2. include same prelim pool int the formal pool 
	# note: include a dummy prelim variable, account for lagged structure
	# note: for sudan libya turn icclevel into zero, because they were referred from unsc