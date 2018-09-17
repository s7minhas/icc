####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

###############################################################
load(paste0(pathData, 'latAngle_MG/logitModData.rda'))
latAngle=modData ; rm(modData)

# subset
latAngle = latAngle[ which(latAngle$year>=1999),
	c('ccode1','ccode2','year','latAngle') ]
###############################################################

###############################################################
# add cname ids
codes = data.frame(code=unique(c(latAngle$ccode1, latAngle$ccode2)))
codes$name = panel$cname[match(codes$code, panel$ccode)]

latAngle$cname1 = codes$name[match(latAngle$ccode1, codes$code)]
latAngle$cname2 = codes$name[match(latAngle$ccode2, codes$code)]

# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
latAngle = latAngle[which(latAngle$cname2 %in% toKeep),]

# spread data
latAngle = latAngle[,c('ccode1','cname1','cname2','year','latAngle')]
latAngle = latAngle %>% 
	gather(variable, value, -(ccode1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)
latAngle[is.na(latAngle)] = NA

# org for calculation
pData = lapply(toKeep, function(v){
	slice = latAngle[which(latAngle$cname1 == v),]
	slice[,which(grepl(v, names(latAngle)))] = NA
	return(slice) })
pData[[length(pData) + 1 ]] = latAngle[which(!latAngle$cname1 %in% toKeep),]

# loop over each using p5Vars function
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_latAngle'), 'latAngle') })

# reorg
latAngle = do.call('rbind', pData)
latAngle$p5_latAngleMin[latAngle$p5_latAngleMin==Inf] = NA
latAngle$p5_latAngleMax[latAngle$p5_latAngleMax==-Inf] = NA
###############################################################

###############################################################
# Save
names(latAngle)[1] = 'ccode'
save(latAngle, file=paste0(pathData, 'latAngle_MG/latAngle.rda'))
###############################################################