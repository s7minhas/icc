###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(
	c(
		'sf','dplyr',
		'rnaturalearth',
		'rgeos',
		'cowplot','hrbrthemes', 'gridExtra', 'Cairo'
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
map$cname[
	map$name_long=='Dem. Rep. Korea'
	] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
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
		icclevel_opp_3 = max(icclevel_opp_3)
		) %>% data.frame()

# merge in
mapData = mapData[match(map$ccode,mapData$ccode),]
map$variable = 'State-Focused ICC Transitions through 2016'
map$value = mapData$icclevel_state_3

x = map
x$variable = 'Opposition-Focused ICC Transitions through 2016'
x$value = mapData$icclevel_opp_3
map = rbind(map, x)
map$variable = factor(map$variable, levels=unique(map$variable))
###############################################################

###############################################################
# viz
mapCols = c("0" = "gray85", "1" = "gray60", "2" = "gray25")
gg = ggplot() + 
	geom_sf(data = map, 
		aes(fill=factor(value)), 
		size = .2, color = "white") +
	labs(x = "", y = "") +
	scale_fill_manual(
		'', 
		values=mapCols,
		labels=c(
			'No ICC\nInvestigation',
			'Reached Preliminary\nStage',
			'Reached Formal\nStage'
			) ) +
	facet_wrap(~variable) +
	theme_light(base_family="Source Sans Pro") +
	theme_ipsum() +
	theme(
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		axis.line = element_blank(),
		panel.background = element_blank(),
		legend.position =  'bottom',
		legend.text = element_text(
			size = 6, family="Source Sans Pro Light"),
        strip.text.x = element_text(size = 9, color='white',
            family="Source Sans Pro Semibold", 
            angle=0, hjust=.05),
        strip.background = element_rect(
        	fill = "#525252", color='#525252')
		)

ggsave(gg, 
	file=paste0(pathGraphics, 'iccMaps.pdf'),
	width=10, height=4, device=cairo_pdf
	)
###############################################################	