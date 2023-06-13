###############################################################
source('~/Research/icc/R/setup.R')
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
buildData = function(data){

	# polity (-2016)
	data=addComponentData(
		baseData=data,
		rdaPath=paste0(pathData, 'polity/polity.rda'),
		objName='polity',
		vars=c( "polity2" ) )

	# wbdata (-2017)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'worldBank/worldBank.rda'),
		objName='worldBank',
		vars=c(
			"gdp", "gdpCap", "gdpGr", "pop", "fdiInGDP", 
			"fdiOutGDP", "aidGNI", 
			"gdpLog", 'gdpCapLog', 'popLog' ) )
	data=addComponentData(
		baseData=data,		
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
		baseData=data,		
		rdaPath=paste0(pathData, 'vdem/vdem.rda'),
		objName='vdem',
		vars=c( "v2juhcind", "v2juncind" ) )	

	# pts (-2016)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'pts/pts.rda'),
		objName='pts',
		vars=c( "pts" ) )	

	# ideal points (-2015)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'Voeten/idPt.rda'),
		objName='idPt',
		vars=c( 
			paste0(p5, '_absidealdiff'),
			'p5_absidealdiffAvg',
			'p5_absidealdiffMin',
			'p5_absidealdiffMax' ) )

	# ged osv rebel (-2016)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'ucdp/ged_osv.rda'),
		objName='gedRebel',
		vars=c( 'osv_rebel', 'osv_rebel_cumul' ) )
	data$lag1_osv_rebel[is.na(data$lag1_osv_rebel)] = 0
	data$lag1_osv_rebel_cumul[is.na(data$lag1_osv_rebel_cumul)] = 0

	# ged osv state (-2016)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'ucdp/ged_osv.rda'),
		objName='gedState',
		vars=c( 'osv_state', 'osv_state_cumul' ) )
	data$lag1_osv_state[is.na(data$lag1_osv_state)] = 0
	data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0

	# a&p intervention (-2016)
	load(paste0(pathData, 'apIntervention/intv.rda'))
	intvVars = names(intv)[-(1:2)]
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'apIntervention/intv.rda'),
		objName='intv',
		vars=intvVars )
	varsToFix = paste0('lag1_', intvVars)
	for(v in varsToFix){ data[is.na(data[,v]),v] = 0 }	

	# atop ally (-2016)
	aVars = c( 
		paste0(p5, '_anyAlly'),
		paste0(p5, '_defAlly'),
		'p5_anyAllyAvg','p5_anyAllyMin', 'p5_anyAllyMax',
		'p5_defAllyAvg', 'p5_defAllyMin', 'p5_defAllyMax'
		)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'atop_ally/ally.rda'),
		objName='ally',
		vars=aVars )
	varsToFix = paste0('lag1_', aVars)
	for(v in varsToFix){ data[is.na(data[,v]),v] = 0 }		

	# trade (-2014)
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'cow_trade/trade.rda'),
		objName='trade',
		vars=c( 
			paste0(p5, '_tradeProp'),
			paste0(p5, '_trade'),		
			'p5_trade',
			'p5_tradeProp'
			 ) )

	# add net lat angle
	data=addComponentData(
		baseData=data,		
		rdaPath=paste0(pathData, 'latAngle_MG/latAngle.rda'),
		objName='latAngle',
		vars=c( 
			paste0(p5, '_latAngle'),
			'p5_latAngleAvg',
			'p5_latAngleMin',
			'p5_latAngleMax'
			 ) )

	return(data)
}
###############################################################

###############################################################
# by separate yearly samples for each dv type

# load data
load(paste0(pathData, 'sampFrameData.rda'))
prelimState = buildData(prelimState)
prelimOpp = buildData(prelimOpp)
formalState = buildData(formalState)
formalOpp = buildData(formalOpp)

# save
save(prelimState, prelimOpp, 
	formalState, formalOpp, 
	file=paste0(pathData, 'mergedData.rda'))
###############################################################

###############################################################
# by separate yearly samples for each dv type

# load data
load(paste0(pathData, 'baseData_yrly_ongoing.rda'))
data = buildData(data)
###############################################################

###############################################################
# create alternative icc level variables

# model 1a
## sequential model with a recoded DV with the 
## following categories: 0=0, 1=1, 2=everything else (2-6)
fx = function(y){x=0*y; x[y==1]=1; x[y>1]=2; return(x)}
data$icclevel_3 = fx(data$icclevel)
data$icclevel_state_3 = fx(data$icclevel_state)
data$icclevel_opp_3 = fx(data$icclevel_opp)

# model 1b
## sequential model with DV recoded into the 
## following categories: 0=0, 1=1, 2=all 2s and 3s, 3=all 4s, 5s, and 6s
fx = function(y){x=0*y; x[y==1]=1; x[y %in% c(2,3)]=2; x[y>3]=3; return(x)}
data$icclevel_4a = fx(data$icclevel)
data$icclevel_state_4a = fx(data$icclevel_state)
data$icclevel_opp_4a = fx(data$icclevel_opp)

# model 1c
## sequential model with DV recoded into the
## following categories: 0=0, 1=1, 2=2, 3=everything else (3=6)
fx = function(y){x=0*y; x[y==1]=1; x[y==2]=2; x[y>2]=3; return(x)}
data$icclevel_4b = fx(data$icclevel)
data$icclevel_state_4b = fx(data$icclevel_state)
data$icclevel_opp_4b = fx(data$icclevel_opp)

# model 2a
## drop icclevel=0
## sequential model with DV recoded into the
## following categories: 0=all 1s, 1=all 2-3s, 3=all 4-6s
fx = function(y){x=0*y; x[y==0]=NA; x[y==1]=1; x[y %in% c(2,3)]=2; x[y>3]=3; return(x)}
data$icclevel2_3a = fx(data$icclevel)
data$icclevel2_state_3a = fx(data$icclevel_state)
data$icclevel2_opp_3a = fx(data$icclevel_opp)

# model 2b
## drop icclevel=0
## sequential model with DV recoded into the 
## following categories: 0=all 1s, 1=all 2s, 3=everything else (3-6)
fx = function(y){x=0*y; x[y==0]=NA; x[y==1]=1; x[y==2]=2; x[y>2]=3; return(x)}
data$icclevel2_3b = fx(data$icclevel)
data$icclevel2_state_3b = fx(data$icclevel_state)
data$icclevel2_opp_3b = fx(data$icclevel_opp)
###############################################################

###############################################################
# save
save(data, 
	file=paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
###############################################################