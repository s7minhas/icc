if(Sys.info()['user'] %in% c('s7m', 'janus829')){
    source('~/Research/icc/R/setup.R') }

############################
# Download file from ICOW site  
tradeURL = 'http://www.correlatesofwar.org/data-sets/bilateral-trade/cow_trade_4.0/at_download/file'
tradeName = paste0(pathData, 'cow_trade/trade.zip')
if(!file.exists(tradeName)) { download.file(tradeURL, tradeName) }

currDir = getwd()
trade = unzip(tradeName, 
	'COW_Trade_4.0/Dyadic_COW_4.0.csv') %>% read.csv()
unlink(paste0(getwd(), '/COW_Trade_4.0'), 
    recursive=TRUE, force=TRUE)	
############################

############################
# Clean COW trade dataset
# Clean Trade [extends from 1870 to 2014]
trade2 <- trade[,c('importer1', 'importer2', 'year', 'flow1', 'flow2')]
colnames(trade2) <- c('state_name1', 'state_name2', 'year', 'imports', 'exports')
trade2 <- trade2[trade2$year>=1999,]

trade2$state_name1 <- char(trade2$state_name1)
trade2$state_name2 <- char(trade2$state_name2)

trade2$imports[trade2$imports==-9] <- 0 # setting missing to 0
trade2$exports[trade2$exports==-9] <- 0 # setting missing to 0

trade2$state_name1[trade2$state_name1=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Federated States of Micr'] <- 'Micronesia'
trade2$state_name2[trade2$state_name2=='Federated States of Micr'] <- 'Micronesia'

trade2 <- trade2[trade2$state_name1!="Yemen People's Republic",]
trade2 <- trade2[trade2$state_name2!="Yemen People's Republic",]

states <- unique(append(trade2$state_name1, trade2$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- char(temp$cname)
temp$cname[temp$cname=='Yugoslavia'] <- 'SERBIA'
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

trade2$cname_1 <- temp$cname[match(trade2$state_name1,temp$states)]
trade2$cname_2 <- temp$cname[match(trade2$state_name2,temp$states)]

trade2$ccode_1 <- temp$ccode[match(trade2$state_name1,temp$states)]
trade2$ccode_2 <- temp$ccode[match(trade2$state_name2,temp$states)]

trade2 <- trade2[!is.na(trade2$ccode_1),]
trade2 <- trade2[!is.na(trade2$ccode_2),]

trade2$cyear_1 <- num(paste0(trade2$ccode_1, trade2$year))
trade2$cyear_2 <- num(paste0(trade2$ccode_2, trade2$year))

# Removing duplicates	
trade2$drop <- 0
# trade2[trade2$state_name1=='German Federal Republic' & trade2$year==1990,]
trade2[trade2$state_name1=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name1=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2 <- trade2[trade2$drop!=1,]; trade2 <- trade2[,c(1:(ncol(trade2)-1))]

### ASIDE
# Create a separate export & import dataset
temp1 <- trade2[,c('ccode_1','ccode_2','year','exports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','exports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','imports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','exports')
exports <- rbind(temp1, temp2)
exports$exports <- exports$exports*1000000

temp1 <- trade2[,c('ccode_1','ccode_2','year','imports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','imports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','exports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','imports')
imports <- rbind(temp1, temp2)
imports$imports <- imports$imports*1000000

trade3 <- cbind(imports[,1:3], trade=imports[,4]+exports[,4])
trade3$cyear_1 <- paste(trade3$ccode_1, trade3$year, sep='')
trade3$cyear_2 <- paste(trade3$ccode_2, trade3$year, sep='')
trade3$cname_1 <- panel$cname[match(trade3$ccode_1, panel$ccode)]
trade3$cname_2 <- panel$cname[match(trade3$ccode_2, panel$ccode)]

# Subsetting to relevant vars
trade <- trade3[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'trade')]
############################

###############################################################
# relabel vars
names(trade)[1:4] = c(paste0('ccode',1:2),paste0('cname',1:2))
trade = trade[,-match(c(paste0('cyear_',1:2)),names(trade))]

# calc proportion of trade by country
tmp = trade %>% group_by(cname1,year) %>%
	summarize(totTrade = sum(trade))

# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
trade = trade[which(trade$cname2 %in% toKeep),-match('ccode2',names(trade))]

# spread data
trade = trade %>% 
	gather(variable, value, -(ccode1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)
trade[is.na(trade)] = 0

# bring in totTrade
trade$totTrade_1 = tmp$totTrade[
	match(
		paste0(trade$cname1, trade$year),
		paste0(tmp$cname1, tmp$year) ) ]

# get p5 vars
trade$p5_trade = apply(trade[,paste0(toKeep,'_trade')], 1, sum)
for(v in names(trade)[c(4:8,10)]){
	trade$tmp = trade[,v]/trade$totTrade_1
	names(trade)[ncol(trade)] = paste0(v, 'Prop') }

# remove some small countries
trade = trade[!is.na(trade$CHINA_tradeProp),]
###############################################################

############################
# Save
names(trade)[1] = 'ccode'
save(trade, 
	file=paste0(pathData, 'cow_trade/trade.rda'))
############################