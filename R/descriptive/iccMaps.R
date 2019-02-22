###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(
	c(
		'sf','dplyr',
		'rnaturalearth',
		'rgeos','raster','cshapes',
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
#
map <- cshp(date = as.Date("2012-12-31")) %>%
  # the map data has some points outside 180 degrees, this causes plotting 
  # issues with projected data
  # raster::crop(., extent(-180, 180, -90, 90)) %>%
  st_as_sf() %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform("+proj=robin") %>%
  st_simplify(dTolerance = 10000, preserveTopology = TRUE) %>%
  dplyr::select("GWCODE", "CNTRY_NAME", "ISO1AL3", "geometry")

# Cshapes maps are missing Greenland and some other places, get a background
# map to show the correct world
bg_map <- ne_countries(scale = 110, type = 'countries') %>%
  st_as_sf() %>%
  st_transform("+proj=robin")

# Main map
p1 <- ggplot() + 
  geom_sf(data = bg_map, size = .2, color = "white") +
  # geom_sf(data = map, aes(fill = age_bins), size = .1, color = "white") +
  geom_sf(data = map, size = .1, color = "white") +
  theme_ipsum() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank()) +
  scale_fill_viridis_d(guide = FALSE, direction = -1) +
  labs(x = "", y = "")    
ggsave(p1, file='~/Desktop/tmp.pdf')
###############################################################	