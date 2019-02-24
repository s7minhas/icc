# marginal effects
plot(
	marginal_effects(
		oppMod, 
		effects='lag1_p5_absidealdiffMin', 
		categorical=TRUE
		)	
	)

load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
x = marginal_effects(
	oppMod, 
	effects='lag1_p5_absidealdiffMin', 
	categorical=TRUE)[[1]]

ggplot(data=x, 
	aes(
		x=lag1_p5_absidealdiffMin, y=estimate__, 
		color=cats__, fill=cats__
		)) + 
	geom_line() + 
	geom_ribbon(aes(ymin=lower__,ymax=upper__),alpha=.5) + 
	# geom_rug(data=data, aes(x=lag1_v2juncind,y=0),sides='b') +
	facet_wrap(~cats__, scales='free_y')