###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop', 'brms', 'future'))
###############################################################

###############################################################
# load existing data, main diff between osv in this
# vs ucdp is that when we started the project ucdp
# didnt have data on syria so alyssa manually collected, 
# but now ucdp has syria data so we can use that
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
###############################################################

###############################################################
# use raw ucdp data to measure osv, without
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
###############################################################

###############################################################
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
###############################################################

###############################################################
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
data$lag1_osv_rebel_cumul = gedRebel$osv_rebel_cumul[
  match(data$cnameYear,gedRebel$cnameYear)]
###############################################################

###############################################################
# save
save(data, file=paste0(pathData, 'modData_fin.rda'))
###############################################################