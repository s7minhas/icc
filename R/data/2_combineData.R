###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################

###############################################################
# load base data
load(paste0(pathData, 'baseData.rda'))
###############################################################

###############################################################
# helper fn to merge data from component files
addComponentData = function(
	baseData=data, rdaPath, 
	objName, vars, lag=TRUE){
	load(rdaPath) ; toAdd = get(objName)

	# merge in vars at same pd
	if(!lag){
		toAdd$ccodeYear = with(toAdd, paste(ccode, year, sep='_'))
		for(v in vars){
			baseData$tmp = toAdd[match(baseData$ccodeYear, toAdd$ccodeYear),v]
			names(baseData)[ncol(baseData)] = v }
	}

	# merge lagged version
	if(lag){
		toAdd$year = toAdd$year + 1
		toAdd$ccodeYear = with(toAdd, paste(ccode, year, sep='_'))
		for(v in vars){
			baseData$tmp = toAdd[match(baseData$ccodeYear, toAdd$ccodeYear),v]
			names(baseData)[ncol(baseData)] = paste0('lag1_', v) }
	}

	return(baseData) }

# p5 countries
p5 = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
###############################################################

# merge variables ##############################################################
# polity (-2016)
data=addComponentData(
	rdaPath=paste0(pathData, 'polity/polity.rda'),
	objName='polity',
	vars=c( "polity2" ) )

# wbdata (-2017)
data=addComponentData(
	rdaPath=paste0(pathData, 'worldBank/worldBank.rda'),
	objName='worldBank',
	vars=c(
		"gdp", "gdpCap", "gdpGr", "pop", "fdiInGDP", 
		"fdiOutGDP", "aidGNI", 
		"gdpLog", 'gdpCapLog', 'popLog' ) )
data=addComponentData(
	rdaPath=paste0(pathData, 'worldBank/worldBank.rda'),
	objName='worldBank',
	vars=c( 'region' ), lag=FALSE )
data$region = char(data$region)
data$africa = 0
data$africa[data$region=="Sub-Saharan Africa "] = 1
other = sort(unique(data$cname[which(data$region=='Middle East & North Africa')]))
moreafrica = c(other[c(1,3,4,11,13,18)], 'NAMIBIA')
data$africa[which(data$cname %in% moreafrica)] = 1
data = data[,-match('region',names(data))]

# vdem (-2017)
data=addComponentData(
	rdaPath=paste0(pathData, 'vdem/vdem.rda'),
	objName='vdem',
	vars=c( "v2juhcind", "v2juncind" ) )	

# pts (-2016)
data=addComponentData(
	rdaPath=paste0(pathData, 'pts/pts.rda'),
	objName='pts',
	vars=c( "pts" ) )	

# ideal points (-2015)
data=addComponentData(
	rdaPath=paste0(pathData, 'Voeten/idPt.rda'),
	objName='idPt',
	vars=c( 
		paste0(p5, '_absidealdiff'),
		'p5_absidealdiffAvg' ) )

# ged osv rebel (-2016)
data=addComponentData(
	rdaPath=paste0(pathData, 'ucdp/ged_osv.rda'),
	objName='gedRebel',
	vars=c( 'osv_rebel' ) )
data$lag1_osv_rebel[is.na(data$lag1_osv_rebel)] = 0

# ged osv state (-2016)
data=addComponentData(
	rdaPath=paste0(pathData, 'ucdp/ged_osv.rda'),
	objName='gedState',
	vars=c( 'osv_state' ) )
data$lag1_osv_state[is.na(data$lag1_osv_state)] = 0

# ged intervention (-2016)
intvVars = c(
	paste0(p5, '_intv_rebel'), 
	paste0(p5, '_intv_state'), 
	paste0(p5, '_intv_rebel_any'), 
	paste0(p5, '_intv_state_any'), 
	paste0('p5_', c(
		'intv_rebel', 'intv_rebel_any', 'intv_rebel_mean', 'intv_rebel_prop',
		'intv_state', 'intv_state_any', 'intv_state_mean', 'intv_state_prop'
		)) )
data=addComponentData(
	rdaPath=paste0(pathData, 'ucdp/ged_intv.rda'),
	objName='gedIntv',
	vars=intvVars )
varsToFix = paste0('lag1_', intvVars)
for(v in varsToFix){ data[is.na(data[,v]),v] = 0 }
###############################################################

###############################################################
# save
save(data, file=paste0(pathData, 'mergedData.rda'))
write.csv(data, file=paste0(pathData, 'mergedData.csv'))
###############################################################