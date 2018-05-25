###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }
###############################################################

###############################################################
#pts data
load(paste0(pathData, 'pts/PTS-2017.rdata'))
###############################################################

###############################################################
# restrict time
pts = PTS_2017[PTS_2017$Year>=2001,]

# match names
pts$cname = cname(pts$Country)

# add ccode
pts$ccode = panel$ccode[match(pts$cname,panel$cname)]

# corrections
pts$ccode[pts$Country=='Czechoslovakia'] = 'CZECH REPUBLIC'
pts$ccode[pts$Country=='Yugoslavia, Federal Republic of'] = 'SERBIA'
pts$ccode[pts$Country=='Yugoslavia, Socialist Federal Republic of'] = 'SERBIA'

# remove EU, Gaza, Palestine, Puerto Rico, Western Sahara
pts = pts[!is.na(pts$ccode),]
pts = pts[!pts$Country %in% 
	c(
		'Israel in Occupied Territories',
		'Israel in pre-1967 borders',
		'Yemen Arab Republic',
		"Yemen People's Republic"
		),
	]
###############################################################

###############################################################
# Save
save( pts,
	file=paste0(pathData, 'pts/pts.rda'))
###############################################################