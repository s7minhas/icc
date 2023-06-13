if(Sys.info()['user'] %in% c('s7m', 'janus829')){
    source('~/Research/icc/R/setup.R') }

############################
# Download file from ICOW site
cincURL = 'http://www.correlatesofwar.org/data-sets/national-material-capabilities/nmc-v5-1/at_download/file'
cincName = paste0(pathData, 'cow_cinc/cinc.zip')
if(!file.exists(cincName)) { download.file(cincURL, cincName) }

cinc = unzip(cincName, 
	'NMC_5_0.dta') %>% read.dta()
############################

############################
# greater than 1999
cinc = cinc[cinc$year>=1999,]

# stdz country names
cinc$cname = countrycode(cinc$stateabb, 'cowc', 'country.name')

# some fixes
cinc$cname[cinc$stateabb=='GFR'] = 'GERMANY'
cinc$cname[cinc$stateabb=='CZE'] = 'CZECH REPUBLIC'
cinc$cname[cinc$stateabb=='YUG'] = 'SERBIA'
cinc$cname[cinc$stateabb=='YAR'] = 'YEMEN'
cinc$cname[cinc$stateabb=='YPR'] = 'S. YEMEN'
cinc$cname[cinc$stateabb=='DRV'] = 'VIETNAM'
cinc$cname[cinc$stateabb=='RVN'] = 'S. VIETNAM'

# bring in ccode
cinc$ccode = panel$ccode[match(cinc$cname, panel$cname)]

# add cyear ids
cinc$ccodeYear = with(cinc, paste0(ccode, year))
cinc$cnameYear = with(cinc, paste0(cname, year))
############################

############################
# save
save(cinc, file=paste0(pathData, 'cow_cinc/cinc.rda'))
############################