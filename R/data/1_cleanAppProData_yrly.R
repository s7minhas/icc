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

# organize by state-year
data = data[order(data$ccodeYear),]
###############################################################

# PRELIM/FORMAL GOV SAMPLE ##############################################################
### sample rule for inclusion in prelim model: 
# 1. once a state experiences a prelim investigation it drops from the dataset in the next year
# 2. it only reenters the dataset once there is no icc involvement
### can create this from just the icc level models

### sample rule for inclusion in formal model: 
# 1. State only enters formal model if it has experienced a prelim in the past
# 2. once a state experiences a formal investigation it drops from the dataset in the next year
# 3. it only reenters the dataset once there is a new prelim
### can create this from just the icc level models

###
vars = c('cname', 'ccodeYear','prelim_icc_state','formal_icc_state')

## remove after a state faces a prelim investigation and stays under icc
iccStates = sort(unique(data$cname[data$prelim_icc_state==1]))

## manually remove irrelevant years for icc states
prelimGov = data

### afghanistan
prelimGov[prelimGov$cname==iccStates[1],vars]
toRemove = paste0('700_',2008:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('700_',2008:2016)
formalGov = data[which(data$ccodeYear %in% toAdd),]

### burundi
prelimGov[prelimGov$cname==iccStates[2],vars]

### colombia
prelimGov[prelimGov$cname==iccStates[3],vars]
toRemove = paste0('100_',2005:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('100_',2005:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

### gabon
prelimGov[prelimGov$cname==iccStates[4],vars]

###  georgia
prelimGov[prelimGov$cname==iccStates[5],vars]
toRemove = paste0('372_',2009:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('372_',2009:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  guinea
prelimGov[prelimGov$cname==iccStates[6],vars]
toRemove = paste0('438_',2010)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('438_',2010)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  honduras
prelimGov[prelimGov$cname==iccStates[7],vars]
toRemove = paste0('91_',2011:2015)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('91_',2011:2015)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  israel
prelimGov[prelimGov$cname==iccStates[8],vars]
toRemove = paste0('666_',2010:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('666_',2010:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  kenya
prelimGov[prelimGov$cname==iccStates[9],vars]
toRemove = paste0('501_',2010:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('501_',2010)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  north korea
prelimGov[prelimGov$cname==iccStates[10],vars]
toRemove = paste0('731_',2011:2014)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('731_',2011:2014)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  libya
prelimGov[prelimGov$cname==iccStates[11],vars]

###  nigeria
prelimGov[prelimGov$cname==iccStates[12],vars]
toRemove = paste0('475_',2011:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('475_',2011:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  russia
prelimGov[prelimGov$cname==iccStates[13],vars]
toRemove = paste0('365_',2009:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('365_',2009:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  sudan
prelimGov[prelimGov$cname==iccStates[14],vars]
toRemove = paste0('625_',2006:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

###  ukraine
prelimGov[prelimGov$cname==iccStates[15],vars]
toRemove = paste0('369_',2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('369_',2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  uk
prelimGov[prelimGov$cname==iccStates[16],vars]
toRemove = c(paste0('200_',2005:2006), paste0('200_',2015:2016))
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = c(paste0('200_',2005:2006), paste0('200_',2015:2016))
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  usa
prelimGov[prelimGov$cname==iccStates[17],vars]
toRemove = paste0('2_',2008:2016)
prelimGov = prelimGov[which(!prelimGov$ccodeYear %in% toRemove),]

toAdd = paste0('2_',2008:2016)
formalGov = rbind(formalGov, data[which(data$ccodeYear %in% toAdd),])

###  venezuela
prelimGov[prelimGov$cname==iccStates[18],vars]

# 2. include same prelim pool int the formal pool 
	# note: include a dummy prelim variable, account for lagged structure
	# note: for sudan libya turn icclevel into zero, because they were referred from unsc

###############################################################

# PRELIM/FORMAL OPP SAMPLE ##############################################################
### sample rule for inclusion in prelim model: 
# 1. once a state experiences a prelim investigation it drops from the dataset in the next year
# 2. it only reenters the dataset once there is no icc involvement
### can create this from just the icc level models

###
vars = c('cname', 'ccodeYear','prelim_icc_opp','formal_icc_opp')

## remove after a state faces a prelim investigation and stays under icc
iccStates = sort(unique(data$cname[data$prelim_icc_opp==1]))

## manually remove irrelevant years for icc states
prelimOpp = data

### afghanistan
prelimOpp[prelimOpp$cname==iccStates[1],vars]
toRemove = paste0('700_',2008:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('700_',2008:2016)
formalOpp = data[which(data$ccodeYear %in% toAdd),]

### central african republic
prelimOpp[prelimOpp$cname==iccStates[2],vars]
toRemove = paste0('482_',2006:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('482_',2006:2007)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

### colombia
prelimOpp[prelimOpp$cname==iccStates[3],vars]
toRemove = paste0('100_',2005:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('100_',2005:2016)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

### drc congo
prelimOpp[prelimOpp$cname==iccStates[4],vars]
toRemove = paste0('490_',2005:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

###  ivory coast
prelimOpp[prelimOpp$cname==iccStates[5],vars]
toRemove = paste0('437_',2012:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

###  gabon
prelimOpp[prelimOpp$cname==iccStates[6],vars]

###  georgia
prelimOpp[prelimOpp$cname==iccStates[7],vars]
toRemove = paste0('372_',2009:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('372_',2009:2016)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  guinea
prelimOpp[prelimOpp$cname==iccStates[8],vars]
toRemove = paste0('438_',2011:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('438_',2011:2016)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  honduras
prelimOpp[prelimOpp$cname==iccStates[9],vars]
toRemove = paste0('91_',2011:2015)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('91_',2011:2015)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  israsel
prelimOpp[prelimOpp$cname==iccStates[10],vars]
toRemove = c(paste0('666_',2010:2012), paste0('666_',2016))
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = c(paste0('666_',2010:2012), paste0('666_',2016))
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  mali
prelimOpp[prelimOpp$cname==iccStates[11],vars]
toRemove = paste0('432_',2013:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('432_',2013)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  nigeria
prelimOpp[prelimOpp$cname==iccStates[12],vars]
toRemove = paste0('475_',2011:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('475_',2011:2016)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  sudan
prelimOpp[prelimOpp$cname==iccStates[13],vars]
toRemove = paste0('625_',2006:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

###  uganda
prelimOpp[prelimOpp$cname==iccStates[14],vars]
toRemove = paste0('500_',2004:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('500_',2004)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  ukraine
prelimOpp[prelimOpp$cname==iccStates[15],vars]
toRemove = paste0('369_',2015:2016)
prelimOpp = prelimOpp[which(!prelimOpp$ccodeYear %in% toRemove),]

toAdd = paste0('369_',2015:2016)
formalOpp = rbind(formalOpp, data[which(data$ccodeYear %in% toAdd),])

###  venezuela
prelimOpp[prelimOpp$cname==iccStates[16],vars]

# 2. include same prelim pool int the formal pool 
	# note: include a dummy prelim variable, account for lagged structure
	# note: for sudan libya turn icclevel into zero, because they were referred from unsc
###############################################################

### dealing with formal pool ... 
# 1. go back down to the monthly level ... only include cases that have experienced prelims
# 2. include same prelim pool int the formal pool 
	# note: include a dummy prelim variable, account for lagged structure
	# note: for sudan libya turn icclevel into zero, because they were referred from unsc
