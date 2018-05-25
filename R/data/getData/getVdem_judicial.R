###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################	

###############################################################	
#vdem
if(!file.exists(paste0(pathData, 'vdem/vdem_judicial.rda'))){
	vdem = read.csv(paste0(pathData, 'vdem/V-Dem-CY+Others-v8.csv'))
	ids = c('country_name', 'country_id', 'year', 'COWcode')
	vars = c(
		names(vdem)[grepl('v2juhcind', names(vdem))],
		names(vdem)[grepl('v2juncind', names(vdem))]
		)
	vdem = vdem[,c(ids, vars)]
	save(vdem, file=paste0(pathData, 'vdem/vdem_judicial.rda'))
} else { load(paste0(pathData, 'vdem/vdem_judicial.rda')) }
###############################################################

###############################################################
# restrict time
vdem = vdem[vdem$year>=2001,]

# match names
vdem$cname = cname(vdem$country_name)

# corrections
vdem$cname[vdem$COWcode==403] = cname('Sao Tome and Principe')

# add ccode
vdem$ccode = panel$ccode[match(vdem$cname,panel$cname)]

# remove hong kong, palestine/west bank, and palestine/gaza
vdem = vdem[!is.na(vdem$ccode),]
###############################################################

###############################################################
# Save
save( vdem,
	file=paste0(pathData, 'vdem/vdem.rda'))
###############################################################