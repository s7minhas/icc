###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################	

###############################################################
# Download file from ICOW site
setwd(paste0(pathData, 'cow_ally'))
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(pathData, 'cow_ally/ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }

ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>%
	foreign::read.dta()
unlink(paste0(getwd(), 'version4.1_dta'), recursive=TRUE, force=TRUE)

# Include only post 1999 data
ally = ally[ally$year>=1999,]

# Subset to relevant vars
ally = ally[,c(
	'ccode1','ccode2','state_name1','state_name2','year',
	'defense', 'neutrality', 'nonaggression', 'entente'
	)]
###############################################################

###############################################################
# Match ally names to panel
cntries = c(ally$state_name1, ally$state_name2) %>%
	char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'

# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to un
ally$cname1 = cntries$cname[match(ally$state_name1, cntries$cntry)]
ally$cname2 = cntries$cname[match(ally$state_name2, cntries$cntry)]
ally$ccode1 = cntries$ccode[match(ally$state_name1, cntries$cntry)]
ally$ccode2 = cntries$ccode[match(ally$state_name2, cntries$cntry)]

# Create separate dfs for defense and any
ally$any = apply(ally[,c('defense', 'neutrality', 'nonaggression', 'entente')], 1, sum) %>% ifelse(., 1, 0)
ally$any_NoNonAgg = apply(ally[,c('defense', 'neutrality', 'entente')], 1, sum) %>% ifelse(., 1, 0)
ally$def = apply(ally[,c('defense'),drop=FALSE], 1, sum) %>% ifelse(., 1, 0)
ally$did = paste(ally$ccode1,ally$ccode2,ally$year, sep='_')

# keep max identified rel between dyad pairs
ally = ally %>% group_by(did, ccode1, cname1, cname2, year) %>%
	summarize(
		defAlly = max(def),
		anyAlly = max(any),
		any_NoNonAggAlly = max(any_NoNonAgg)		
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
	p5Vars(x, paste0(toKeep,'_any_NoNonAggAlly'), 'any_NoNonAggAlly') })
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_defAlly'), 'defAlly') })

# reorg
ally = do.call('rbind', pData)
###############################################################

###############################################################
# Save
names(ally)[1] = 'ccode'
save( ally,
	file=paste0(pathData, 'cow_ally/ally.rda'))
###############################################################