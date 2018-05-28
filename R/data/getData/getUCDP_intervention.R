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
# limit to year and vars of interest
vars = c('year','type_of_vi',
	'side_a','gwnoa','side_b','gwnob',
	'country','best'
	)
ged = ged[which(ged$year>=1999),vars]
ged = ged[which(ged$type_of_vi==1),]

# limit to events involving p5 sender
toKeep = c(
	'Government of United States of America',
	'Government of United Kingdom',
	'Government of Australia, Government of United Kingdom, Government of United States of America',
	'Government of China',
	'Government of Russia (Soviet Union)',
	'Government of France' # no events .... weird.
	)
ged = ged[!is.na(ged$gwnoa),]
ged = ged[which(ged$side_a %in% toKeep),]

# separate out joint aus-uk-us events
slice = ged[ged$side_a == toKeep[3],]
tmpUSA = slice ; tmpUSA$side_a = toKeep[1] ; tmpUSA$gwnoa = 2
tmpUK = slice ; tmpUK$side_a = toKeep[2] ; tmpUK$gwnoa = 200
ged = ged[ged$side_a != toKeep[3],] 
ged = rbind(ged, tmpUSA, tmpUK)

# add country ids for side a
ged$cnamea = cname(trim(gsub('Government of', '', ged$side_a)))
ged$ccodea = panel$ccode[match(ged$cnamea, panel$cname)]

# add location 
ged$cname = cname(ged$country)
ged$ccode = panel$ccode[match(ged$cname, panel$cname)]

# remove internal
ged = ged[which(ged$cnamea!=ged$cname),]
############################

############################
# prep to spread
ged = ged[,c(
	'ccode','cname','year',
	'cnamea'
	)]
ged$intervention = 1

# agg by ids
gedIntv = ged %>%
	group_by(ccode,cname,year,cnamea) %>%
	summarize(
		intvCount=sum(intervention)
		) %>%
	data.frame(.,stringsAsFactors = FALSE)
gedIntv$intvAny = ifelse(gedIntv$intvCount>0,1,0)

# spread data
gedIntv = gedIntv %>% 
	gather(variable, value, -(ccode:cnamea)) %>%
	unite(temp, cnamea, variable) %>%
	spread(temp, value)
gedIntv[is.na(gedIntv)] = 0
gedIntv$FRANCE_intvCount = 0 #ugh ucdp
gedIntv$FRANCE_intvAny = 0 #ugh ucdp

# get p5 vars
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
gedIntv$p5_intvCountSum = apply(gedIntv[,paste0(toKeep,'_intvCount')], 1, sum)
gedIntv$p5_intvAnySum = apply(gedIntv[,paste0(toKeep,'_intvAny')], 1, sum)

# calc proportions
denom = rep(5,nrow(gedIntv))
denom = ifelse(gedIntv$cname %in% toKeep, 4, 5)
gedIntv$p5_intvCountMean = gedIntv$p5_intvCountSum/denom
gedIntv$p5_intvAnyProp = gedIntv$p5_intvAnySum/denom
############################

############################
# save
save(gedIntv, file=paste0(pathData, 'ucdp/ged_intv.rda'))
############################