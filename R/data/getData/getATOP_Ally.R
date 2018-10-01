###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################	

###############################################################
# Download file from ICOW site
setwd(paste0(pathData, 'atop_ally/ATOP V4.0 Data (dta)/'))
ally = read.dta('atop4_0ddyr.dta')

# Include only post 1999 data
ally = ally[ally$year>=1999,]

# Subset to relevant vars
# not including offense because there are no
# offensive alliances post 1990
ally = ally[,c(
	'stateA','stateB','year',
	'atopally', 'defense', 'neutral', 'nonagg'
	)]
names(ally)[1:2] = c('cowcode1','cowcode2')
###############################################################

###############################################################
# Match ally names to panel
ckey = data.frame(
	dirty=unique(c(ally$cowcode1,ally$cowcode2)), 
	stringsAsFactors = FALSE)
ckey$clean = panel$cname[match(ckey$dirty, panel$COWCODE)]
ckey$ccode = panel$ccode[match(ckey$clean, panel$cname)]

ally$cname1 = ckey$clean[match(ally$cowcode1,ckey$dirty)]
ally$cname2 = ckey$clean[match(ally$cowcode2,ckey$dirty)]

# remove NAs (ccode=533)
ally = na.omit(ally)

# add in final numeric ids
ally$ccode1 = ckey$ccode[match(ally$cname1, ckey$clean)]
ally$ccode2 = ckey$ccode[match(ally$cname2, ckey$clean)]

# Create separate dfs for defense and any
ally$did = paste(ally$ccode1,ally$ccode2,ally$year, sep='_')

# keep max identified rel between dyad pairs
ally = ally %>% group_by(did, ccode1, cname1, cname2, year) %>%
	summarize(
		defAlly = max(defense),
		anyAlly = max(atopally)
		) %>% data.frame(.,stringsAsFactors=FALSE)
###############################################################

###############################################################
# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
ally = ally[which(ally$cname2 %in% toKeep),-1]

# spread data
ally = ally %>% 
	gather(variable, value, -(ccode1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)
ally[is.na(ally)] = 0

# org for calculation
pData = lapply(toKeep, function(v){
	slice = ally[which(ally$cname1 == v),]
	slice[,which(grepl(v, names(ally)))] = NA
	return(slice) })
pData[[length(pData) + 1 ]] = ally[which(!ally$cname1 %in% toKeep),]

# loop over each using p5Vars function
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_anyAlly'), 'anyAlly') })
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_defAlly'), 'defAlly') })

# reorg
ally = do.call('rbind', pData)
###############################################################

###############################################################
# Save
names(ally)[1] = 'ccode'
save( ally,
	file=paste0(pathData, 'atop_ally/ally.rda'))
###############################################################