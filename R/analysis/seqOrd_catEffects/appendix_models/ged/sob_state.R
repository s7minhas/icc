###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop', 'brms', 'future'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',	
  # p5 vars: 
  'lag1_p5_absidealdiffMin' )

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
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
gedState = ged[!is.na(ged$gwnoa),]

# match country names for state
gedState$cname = cname(gedState$side_a)
gedState$cname[gedState$side_a=='Government of Mali'] = cname('Mali')

# aggregate counts by unit year
gedState = gedState %>%
	group_by(cname,year) %>%
	summarize(civDeaths=sum(deaths_civ)) %>%
  data.frame(.,stringsAsFactors = FALSE)

# create running sum of osv variables
gedState = lapply(unique(gedState$cname), function(x){
	slice = gedState[which(gedState$cname==x),]
	slice = slice[order(slice$year),]
	slice$civDeaths_runSum = cumsum(slice$civDeaths)	
	return(slice) }) %>% do.call('rbind', .)

#
names(gedState)[c(3:4)] = c('osv_state', 'osv_state_cumul')

# merge back into data object in place of current var
# and make sure to lag appropriately
gedState$year = gedState$year + 1
gedState$cnameYear = with(gedState, paste(cname, year, sep='_'))

# merge in place of existing var
data$lag1_osv_state_cumul = gedState$osv_state_cumul[
  match(data$cnameYear,gedState$cnameYear)]

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
###############################################################

###############################################################
# org data
data = data[,c(
  'ccode','cname','year',
  'icclevel_state_3',
  sobStateVars )]

# listwise deletion
frame = na.omit(data)

# formatting
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
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
sobStateVars[c(5:8)] = paste0('cs(',sobStateVars[c(5:8)],')')

# run model
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ',
		paste(sobStateVars, collapse = ' + ') ) )
mod = brm(
	formula=sobStateForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
	)
save(
  mod, 
  file=paste0(pathResults, 'sobState_ucdp_fin.rda'))
###############################################################