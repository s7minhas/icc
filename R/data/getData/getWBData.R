if(Sys.info()['user'] %in% c('s7m','janus829')){
  source('~/Research/irNeuralNets/code/R/setup.R') }

############################
# Download WB data using WDI package

# File name to store data
fName = paste0(pathDataRaw, 'worldBankVars.csv')
wbVars = c(
	'NY.GDP.MKTP.KD', # GDP, constant US
	'NY.GDP.PCAP.KD', # GDP per capita, current US
	'NY.GDP.MKTP.KD.ZG', # GDP growth
	'SP.POP.TOTL', # Population
	'SP.POP.1519.MA.5Y', # male pop 15-19
	'SP.POP.2024.MA.5Y', # male pop 20-24
	'SP.POP.2529.MA.5Y', # male pop 25-29
	'SP.POP.1519.FE.5Y', # female pop 15-19
	'SP.POP.2024.FE.5Y', # female pop 20-24
	'SP.POP.2529.FE.5Y', # female pop 25-29	
	'SP.DYN.IMRT.IN', # infant mortality per 1000
	'BX.KLT.DINV.WD.GD.ZS', # Foreign direct investment, net inflows (% of GDP)
	'BM.KLT.DINV.WD.GD.ZS', # Foreign direct investment, net outflows (% of GDP)
	'DT.ODA.ODAT.GN.ZS', # Net ODA received (% of GNI)
	'TX.VAL.FUEL.ZS.UN' # fuel exports % of exports
	)

# Call WDI website
wbData = WDI(country='all', 
	indicator=wbVars, 
	start=1960, end=2018, extra=TRUE )
write.csv(wbData, file=fName)

# Change names
wbVarsClean = c('gdp', 'gdpCap', 'gdpGr', 'pop', 
	'pop1519M', 'pop2024M', 'pop2529M', 
	'pop1519F', 'pop2024F', 'pop2529F', 
	'infantMortality', 'fdiInGDP', 'fdiOutGDP', 'aidGNI', 'fuelExports')
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