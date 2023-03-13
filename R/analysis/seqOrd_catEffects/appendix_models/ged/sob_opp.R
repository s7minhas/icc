###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop','brms', 'future'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_rebel_cumul',
	# p5 vars:
	'lag1_p5_absidealdiffMin' )
###############################################################

###############################################################
# use raw ucdp data to measure state cumul osv, without
# ap's syria correction
# object loaded in: GEDEvent_v22_1
load(paste0(pathData, 'GEDEvent_v22_1.RData'))
ged = GEDEvent_v22_1 ; rm(GEDEvent_v22_1)

# type of violence==3
ged = ged[ged$type_of_vi==3,]

# cleanup
ged = ged[,c('year','side_a','gwnoa','country','deaths_civilians')]
names(ged)[ncol(ged)] = 'deaths_civ'
for(v in c(1,3,5)){ ged[,v] = num(ged[,v])}
for(v in c(2,4)){ ged[,v] = char(ged[,v])}

# separate by state and non-state
gedRebel = ged[is.na(ged$gwnoa),]

# match country names for rebel
gedRebel$cname = cname(gedRebel$country)

# aggregate counts by unit year
gedRebel = gedRebel %>%
	group_by(cname,year) %>%
	summarize(civDeaths=sum(deaths_civ)) %>%
	data.frame(.,stringsAsFactors = FALSE)

# create running sum of osv variables
gedRebel = lapply(unique(gedRebel$cname), function(x){
	slice = gedRebel[which(gedRebel$cname==x),]
	slice = slice[order(slice$year),]
	slice$civDeaths_runSum = cumsum(slice$civDeaths)	
	return(slice) }) %>% do.call('rbind', .)

#
names(gedRebel)[c(3:4)] = c('osv_rebel', 'osv_rebel_cumul')

# merge back into data object in place of current var
# and make sure to lag appropriately
gedRebel$year = gedRebel$year + 1
gedRebel$cnameYear = with(gedRebel, paste(cname, year, sep='_'))

# merge in place of existing var
data$lag1_osv_state_cumul = gedRebel$osv_state_cumul[
  match(data$cnameYear,gedRebel$cnameYear)]

# var transformations
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)
data$lag1_osv_rebel_cumul[is.na(data$lag1_osv_rebel_cumul)] = 0
###############################################################

###############################################################
# org data
data = data[,c(
	'ccode','cname','year',
	'icclevel_opp_3',
	sobOppVars )]

# listwise deletion
frame = na.omit(data)

# formatting
frame$icclevel_opp_3 = as.integer(frame$icclevel_opp_3 + 1)
frame$ccode = as.integer(frame$ccode)
###############################################################

###############################################################
# create p5 variable (2, 365, 220, 710, 200)
frame$p5 = ifelse(
	frame$ccode %in% c(2, 365, 220, 710, 200), 0, 1 )

# modify p5 var to be zero if p5 country
frame$lag1_p5_absidealdiffMin = frame$p5*frame$lag1_p5_absidealdiffMin
###############################################################

###############################################################
# category specific effects
sobOppVars[c(5:8)] = paste0('cs(',sobOppVars[c(5:8)],')')

# run model
sobOppForm = formula(
	paste0('icclevel_opp_3 ~ ',
		paste(sobOppVars, collapse = ' + ') ) )
mod = brm(
	formula=sobOppForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
	)
save(
	mod, 
	file=paste0(pathResults, 'sobOpp_ucdp_fin.rda'))
###############################################################