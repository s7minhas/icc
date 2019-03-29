###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg('lubridate')
###############################################################

###############################################################
# load in AP data and keep only their key vars
# will rebuild rest in subseq files
orig  = read_dta(
	paste0(pathData, 'icc_from_ap/iccdata_013018.dta')
	)

# civilwar fix
orig$civilwar[is.na(orig$civilwar)] = 0

vars = c(
	'icclevel',
	'icclevel_state',
	'icclevel_opp',
	'osv_rebel','osv_state','osv_total',
	'pts',
	'poi_osv_rebel','poi_osv_state','poi_osv_total',
	'poi_pts',
	'civilwar',
	'africa',
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

# add month + 1 for lagging
apData$dateLag = apData$date
month(apData$dateLag) = month(apData$dateLag) + 1
apData$cdateLag = paste(apData$cname, apData$dateLag, sep='_')

# reorder data
apData = apData[,c(
	'cname',
	'cdate','date','cdateLag','dateLag',
	ids,vars)]
###############################################################

###############################################################
# create some helper vars
data = apData %>% 
	dplyr::mutate(
		prelim_icc = ifelse(any(icclevel==1), 1, 0),
		formal_icc = ifelse(any(icclevel>1), 1, 0),
		prelim_icc_state = ifelse(any(icclevel_state==1), 1, 0),
		formal_icc_state = ifelse(any(icclevel_state>1), 1, 0),
		prelim_icc_opp = ifelse(any(icclevel_opp==1), 1, 0),
		formal_icc_opp = ifelse(any(icclevel_opp>1), 1, 0),
		icc_stage1 = ifelse(any(icc_stage1>=1), 1, 0),
		icc_stage2 = ifelse(any(icc_stage2>=1), 1, 0),
		icc_onset = ifelse(any(icc_onset>=1), 1, 0)
		) %>% data.frame()

# add year level ids (useful for merging yearly data)
data$cnameYear = with(data, paste(cname, year, sep='_'))
data$ccodeYear = with(data, paste(ccode, year, sep='_'))
###############################################################

###############################################################
# create running sum of osv variables
data = lapply(unique(data$cname), function(x){
	slice = data[which(data$cname==x),]
	slice = slice[order(slice$date),]
	slice$osv_rebel_cumul = cumsum(slice$osv_rebel)
	slice$osv_state_cumul = cumsum(slice$osv_state)
	slice$osv_total_cumul = cumsum(slice$osv_total)
	return(slice) }) %>% do.call('rbind', .)
###############################################################

###############################################################
# add lagged versions of poi and civilwar var
vars = c(
	"osv_rebel", "osv_state", "osv_total", 
	"osv_rebel_cumul", "osv_state_cumul", "osv_total_cumul", 
	'poi_osv_rebel', 'poi_osv_state','poi_osv_total',
	"pts", 'poi_pts', "civilwar" )

for(v in vars){
	data$tmp = data[match(data$cdate, data$cdateLag),v]
	names(data)[ncol(data)] = paste0('lag1_', v) }
###############################################################

###############################################################
# subset to temporal frame
# if based on dv we'd go through 2016
# however because of if restrictions we go 
# to 12/2015
data = data[
	which(
		data$date > as.Date('2002-06-01', format='%Y-%m-%d') &
		data$date < as.Date('2016-12-01', format='%Y-%m-%d')
		)
	,]
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
save(data, file=paste0(pathData, 'baseData_mnthly_ongoing.rda'))
###############################################################