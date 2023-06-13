####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

############################
# battle deaths conflict level data
load(paste0(pathData, 'ucdp/ged171.Rdata'))
ged = ged171@data
############################

############################
# 
ged = ged[ged$year>=1999,]

# type of violence==3
ged = ged[ged$type_of_vi==3,]

# cleanup
ged = ged[,c('year','side_a','gwnoa','country','deaths_civ')]
for(v in c(1,3,5)){ ged[,v] = num(ged[,v])}
for(v in c(2,4)){ ged[,v] = char(ged[,v])}

# separate by state and non-state
gedState = ged[!is.na(ged$gwnoa),]
gedRebel = ged[is.na(ged$gwnoa),]
############################

############################
# match country names for state
gedState$cname = cname(gedState$side_a)
gedState$cname[gedState$side_a=='Government of Mali'] = cname('Mali')

# aggregate counts by unit year
gedState = gedState %>%
	group_by(cname,year) %>%
	summarize(civDeaths=sum(deaths_civ)) %>% data.frame(.,stringsAsFactors = FALSE)

# add ccodes
gedState$ccode = panel$ccode[match(gedState$cname, panel$cname)]

# make correction for syria based on ap's do file
gedState = rbind(
	gedState,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2001:2010, civDeaths=0, ccode=652
		) )
gedState = rbind(
	gedState,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2011, civDeaths=244, ccode=652
		) )
gedState = rbind(
	gedState,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2012, civDeaths=28, ccode=652
		) )
gedState = rbind(
	gedState,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2013, civDeaths=51, ccode=652
		) )
gedState = rbind(
	gedState,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2013:2016, civDeaths=3, ccode=652
		) )
############################

############################
# match country names for rebel
gedRebel$cname = cname(gedRebel$country)

# aggregate counts by unit year
gedRebel = gedRebel %>%
	group_by(cname,year) %>%
	summarize(civDeaths=sum(deaths_civ)) %>% data.frame(.,stringsAsFactors = FALSE)

# add ccodes
gedRebel$ccode = panel$ccode[match(gedRebel$cname, panel$cname)]

# make correction for syria based on ap's do file
gedRebel = rbind(
	gedRebel,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2001:2012, civDeaths=0, ccode=652
		) )
gedRebel = rbind(
	gedRebel,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2013, civDeaths=73, ccode=652
		) )
gedRebel = rbind(
	gedRebel,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2014, civDeaths=47, ccode=652
		) )
gedRebel = rbind(
	gedRebel,
	data.frame(
		cname='SYRIAN ARAB REPUBLIC',year=2015:2016, civDeaths=213, ccode=652
		) )
############################

############################
# create running sum of osv variables
gedState = lapply(unique(gedState$cname), function(x){
	slice = gedState[which(gedState$cname==x),]
	slice = slice[order(slice$year),]
	slice$civDeaths_runSum = cumsum(slice$civDeaths)	
	return(slice) }) %>% do.call('rbind', .)

gedRebel = lapply(unique(gedRebel$cname), function(x){
	slice = gedRebel[which(gedRebel$cname==x),]
	slice = slice[order(slice$year),]
	slice$civDeaths_runSum = cumsum(slice$civDeaths)	
	return(slice) }) %>% do.call('rbind', .)
############################

############################
# save
names(gedRebel)[c(3,5)] = c('osv_rebel', 'osv_rebel_cumul')
names(gedState)[c(3,5)] = c('osv_state', 'osv_state_cumul')
save(gedRebel, gedState, file=paste0(pathData, 'ucdp/ged_osv.rda'))
############################