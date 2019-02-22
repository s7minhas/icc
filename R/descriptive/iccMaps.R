###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(
	c(
		'sf','dplyr',
		'rnaturalearth',
		'rgeos',
		'cowplot','hrbrthemes'
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

# viz
p1 <- ggplot() + 
  geom_sf(data = map, aes(fill=icc_level_state_3), size = .2, color = "white") +
  ## geom_sf(data = map, aes(fill = age_bins), size = .1, color = "white") +
  # geom_sf(data = map, size = .1, color = "white") +
  theme_ipsum() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank()) +
  scale_fill_viridis_d(guide = FALSE, direction = -1) +
  labs(x = "", y = "")    
p1
###############################################################	