###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(
	c(
		'sf','dplyr',
		'rnaturalearth',
		'rgeos',
		'cowplot','hrbrthemes', 'gridExtra'
		))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

#
data = data[,
	c(
		'cname','ccode','year',
		'icclevel_state_3','icclevel_opp_3', 'icclevel_3'
	),]
###############################################################

###############################################################
# quick base map using rnaturalearth
map = ne_countries(scale = 110, type = 'countries') %>%
  st_as_sf() %>%
  st_transform("+proj=robin")

# create common id to merge in data
map$cname = countrycode(map$name_long, 'country.name', 'country.name')
map$cname[map$name_long=='Dem. Rep. Korea'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
map$cname[map$name_long=='Northern Cyprus'] = NA
map$cname[map$name_long=='Somaliland'] = NA

# add ccode
map$ccode = panel$ccode[match(map$cname, panel$cname)]

# remove geo units with NA
map = map[!is.na(map$ccode),]

# choose what to plot
mapData = data %>% dplyr::group_by(ccode) %>% 
	dplyr::summarize(
		icclevel_state_3 = max(icclevel_state_3),
		icclevel_opp_3 = max(icclevel_opp_3),
		icclevel_3 = max(icclevel_3)
		) %>% data.frame()

# merge in
for(ii in 2:ncol(mapData)){
	map$tmp = mapData[match(map$ccode,mapData$ccode),ii]
	names(map)[ncol(map)] = names(mapData)[ii] }

# break up icclevel into stages
map$stateStage1 = map$icclevel_state_3
map$stateStage1[map$icclevel_state_3>1] = 1

map$stateStage2 = map$icclevel_state_3
map$stateStage2[map$icclevel_state_3<2] = 0

map$oppStage1 = map$icclevel_opp_3
map$oppStage1[map$icclevel_opp_3>1] = 1

map$oppStage2 = map$icclevel_opp_3
map$oppStage2[map$icclevel_opp_3<2] = 0
###############################################################

###############################################################
# viz
mapCols = c("0" = "gray80", "1" = "gray50", "2" = "gray25")
makeMap = function(mapForPlot, colorVar, colorVector=mapCols, mapTitle=NULL){
	mapForPlot$mapColor = colorVar
	p1 = ggplot() + 
		geom_sf(data = mapForPlot, 
			aes(fill=factor(mapColor)), 
			size = .2, color = "white") +
		labs(x = "", y = "") +
		scale_fill_manual(values=colorVector) +
		theme_ipsum() +
		theme(
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			axis.line = element_blank(),
			panel.background = element_blank(),
			legend.position = 'none',
			plot.title = element_text(
				size = 8
				)
		) + 
		ggtitle(mapTitle)
	return(p1)	
}

## note from alyssa
# one thing - can you add dates to the maps?
# just to make it clear that these are only ICC onsets through 2016?
# b/c there are a few more now (like the Philippines) that aren't
# on here b/c our data end before they started.
s1 = makeMap(map, map$stateStage1, mapTitle='ICC Preliminary State')
s2 = makeMap(map, map$stateStage2, mapTitle='ICC Formal State')
o1 = makeMap(map, map$oppStage1, mapTitle='ICC Preliminary Rebel')
o2 = makeMap(map, map$oppStage2, mapTitle='ICC Formal Rebel')

gg=grid.arrange(s1,s2,o1,o2, nrow=2, ncol=2)


ggsave(gg, file='~/Desktop/tmp.pdf', width=10, height=4, device=cairo_pdf)
###############################################################	