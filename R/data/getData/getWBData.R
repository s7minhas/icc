####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

############################
# Download WB data using WDI package
loadPkg('WDI')
# File name to store data
fName = paste0(pathData, 'worldBank/worldBankVars.csv')
wbVars = c(
	'NY.GDP.MKTP.KD', # GDP, constant US
	'NY.GDP.PCAP.KD', # GDP per capita, current US
	'NY.GDP.MKTP.KD.ZG', # GDP growth
	'SP.POP.TOTL', # Population
	'BX.KLT.DINV.WD.GD.ZS', # Foreign direct investment, net inflows (% of GDP)
	'BM.KLT.DINV.WD.GD.ZS', # Foreign direct investment, net outflows (% of GDP)
	'DT.ODA.ODAT.GN.ZS' # Net ODA received (% of GNI)
	)

# Call WDI website
wbData = WDI(country='all', 
	indicator=wbVars, 
	start=1960, end=2018, extra=TRUE )
write.csv(wbData, file=fName)

# Change names
wbVarsClean = c('gdp', 'gdpCap', 'gdpGr', 'pop', 
	'fdiInGDP', 'fdiOutGDP', 'aidGNI')
names(wbData)[4:(length(wbVars)+3)] = wbVarsClean
############################

############################
# Process WB data

# Create matching countrynames
wbData$cname = countrycode(wbData$iso2c, 'iso2c', 'country.name')

# Drop aggregated WB units
wbData = wbData[!is.na(wbData$cname),]

# Create country + year id
wbData$cnameYear = paste0(wbData$cname, wbData$year)

# Check duplicates
table(wbData$cnameYear)[table(wbData$cnameYear)>1]

# Add countrycodes
wbData$ccode = panel$ccode[match(wbData$cname, panel$cname)]

# Drop small islands mostly
wbData = wbData[!is.na(wbData$ccode),]

# Create ccode + year id
wbData$cyear = paste0(wbData$ccode, wbData$year)

# Check duplicates
table(wbData$cyear)[table(wbData$cyear)>1]
############################

############################
# Create logged version of vars
wbData$gdpLog = log(wbData$gdp)
wbData$gdpCapLog = log(wbData$gdpCap)
wbData$popLog = log(wbData$pop)
############################

############################
# Save
worldBank = wbData
save(worldBank, file=paste0(pathDataBin, 'worldBank.rda'))
############################