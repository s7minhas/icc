####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

###############################################################
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/L716E8
load(paste0(pathData,'lji/LJI-estimates-20140422.rdata')); lji=x ; rm(list='x')
###############################################################

###############################################################
# match names
names(lji) = gsub('.','',gsub('X','',names(lji)),fixed=TRUE)

# match to panel
lji$cname = cname(lji$country)

# clean up some names
lji$cname[lji$cname=='Czechoslovakia'] = "CZECH REPUBLIC"
lji$cname[lji$cname=='Yugoslavia'] = "SERBIA"

# add ccode
lji$ccode = panel$ccode[match(lji$cname,panel$cname)]

# remove NAs, gets rid of hong kong
lji = lji[!is.na(lji$ccode),]

# add cyear ids
lji$ccodeYear = paste0(lji$ccode, lji$year)
lji$cnameYear = paste0(lji$cname, lji$year)
###############################################################

###############################################################
# save
save(lji, file=paste0(pathData, 'lji/lji.rda'))
###############################################################