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
addComponentData = function(rdaPath, missPath, objName, vars){
	load(rdaPath) ; toAdd = get(objName)
	data[data$cname %in% unique(setdiff(data$cname, toAdd$cname)),] %>%
		dplyr::group_by(cname) %>%
		dplyr::summarize(
			any_prelim_icc=mean(prelim_icc),
			any_formal_icc=mean(formal_icc),
			any_prelim_icc_state=mean(prelim_icc_state),
			any_formal_icc_state=mean(formal_icc_state),			
			any_prelim_icc_opp=mean(prelim_icc_opp),
			any_formal_icc_opp=mean(formal_icc_opp)
			) %>%
		dplyr::mutate(
			any_prelim_icc = ifelse(any_prelim_icc, 1, 0),
			any_formal_icc = ifelse(any_formal_icc, 1, 0),
			any_prelim_icc_state = ifelse(any_prelim_icc_state, 1, 0),
			any_formal_icc_state = ifelse(any_formal_icc_state, 1, 0),			
			any_prelim_icc_opp = ifelse(any_prelim_icc_opp, 1, 0),
			any_formal_icc_opp = ifelse(any_formal_icc_opp, 1, 0)
			) %>%
		write.csv(., file=missPath)

	# merge in vars at same pd
	toAdd$ccodeYear = with(toAdd, paste(ccode, year, sep='_'))
	for(v in vars){
		data$tmp = toAdd[match(data$ccodeYear, toAdd$ccodeYear),v]
		names(data)[ncol(data)] = v }

	# merge lagged version
	toAdd$year = toAdd$year + 1
	toAdd$ccodeYear = with(toAdd, paste(ccode, year, sep='_'))
	for(v in vars){
		data$tmp = toAdd[match(data$ccodeYear, toAdd$ccodeYear),v]
		names(data)[ncol(data)] = paste0('lag1_', v) }

	return(data) }
###############################################################	

# nodal variables ##############################################################

## icrg (1984-2014)
data = addComponentData(
	rdaPath=paste0(pathData, "icrg/icrg.rda"),
	missPath=paste0(pathData, 'icrg/missing_in_icrg.csv'),
	objName='icrg',
	vars=c(
		"govtStab","socEconCon", "invProf", "intConf", 
		"extConf","corr", "milPol", "relPol","lawOrd",  
		"ethTens","demAcct", "burQual")
	)

# lji (1948-2012)
data = addComponentData(
	rdaPath=paste0(pathData, 'lji/lji.rda'),
	missPath=paste0(pathData, 'lji/missing_in_lji.csv'),
	objName='lji',
	vars=c('LJI','postsd')
	)

# polity (1960-2016)
data=addComponentData(
	rdaPath=paste0(pathData, 'polity/polity.rda'),
	missPath=paste0(pathData, 'polity/missing_in_polity.csv'),
	objName='polity',
	vars=c(
		"polity2", "xrreg", "xrcomp", "xropen", "xconst", 
		"parreg", "parcomp", "exrec", "exconst", "polcomp"
		)
	)

# wbdata (1960-2017)
data=addComponentData(
	rdaPath=paste0(pathData, 'worldBank/worldBank.rda'),
	missPath=paste0(pathData, 'worldBank/missing_in_worldBank.csv'),
	objName='worldBank',
	vars=c(
		"gdp", "gdpCap", "gdpGr", "pop", "fdiInGDP", 
		"fdiOutGDP", "aidGNI", "region", "income", 
		"gdpLog", 'gdpCapLog', 'popLog'
		)
	)
data = data[,-match(c('lag1_region','lag1_income'), names(data))]

# ucdp
###############################################################