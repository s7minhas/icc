if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }

###############################################################
# Get distance matrices in parallel
# Parameters for parallelization
loadPkg('cshapes')
cl = makeCluster(8)
registerDoParallel(cl)
yrs = 2000:2016

# Get capdist mats
print('Collecting capital distance matrices...')
capMats = foreach(yr = yrs, .packages=c("cshapes")) %dopar% {
	if(yr==2016){
		distmatrix(as.Date(paste0(yr, "-6-30")), type="capdist", useGW=TRUE)
	} else {
		distmatrix(as.Date(paste0(yr, "-12-30")), type="capdist", useGW=TRUE)
	}
}

# Get centdist mats
print('Collecting centroid distance matrices...')
centMats = foreach(yr = yrs, .packages=c("cshapes")) %dopar% {
	if(yr == 2016){
		distmatrix(as.Date(paste0(yr, "-6-30")), type="centdist", useGW=TRUE)
	} else {
		distmatrix(as.Date(paste0(yr, "-12-30")), type="centdist", useGW=TRUE)
	}
}

# Get mindist mats
print('Collecting minimum distance matrices...')
minMats = foreach(yr = yrs, .packages=c("cshapes")) %dopar% {
	if(yr == 2016){
		distmatrix(as.Date(paste0(yr, "-6-30")), type="mindist", useGW=TRUE)
	} else {
		distmatrix(as.Date(paste0(yr, "-12-30")), type="mindist", useGW=TRUE)
	}
}
###############################################################

###############################################################
# Clean up each
# Convert GW codes to panel codes
matchPanelCode = function(x){
	pcodes=panel$ccode[match(rownames(x), panel$GWCODE)]
	rownames(x)=colnames(x)=pcodes
	return(x) }

capMats = lapply(capMats, matchPanelCode)
centMats = lapply(centMats, matchPanelCode)
minMats = lapply(minMats, matchPanelCode)

# Label years
names(capMats) = yrs; names(centMats) = yrs; names(minMats) = yrs
###############################################################

###############################################################
# melt into dd frames
distance = melt(capMats)
names(distance) = c('ccode_1','ccode_2','capitalDistance','year')
distance$id = with(distance, paste(ccode_1, ccode_2, year, sep='_'))

tmp=melt(centMats) %>% mutate(id=paste(Var1, Var2, L1, sep='_'))
distance$centDistance = tmp$value[match(distance$id, tmp$id)]

tmp=melt(minMats) %>% mutate(id=paste(Var1, Var2, L1, sep='_'))
distance$minDistance = tmp$value[match(distance$id, tmp$id)]

# cleanup
distance = distance[distance$ccode_1!=distance$ccode_2,]
###############################################################

###############################################################
# var transformations
for(var in names(distance)[c(3,6:7)]){
	distance$tmp = log(distance[,var] + 1)
	names(distance)[ncol(distance)] = paste0(var, 'Log') }
###############################################################

###############################################################
# relabel vars
names(distance)[1:2] = paste0('ccode',1:2)
distance$cname1 = panel$cname[match(distance$ccode1, panel$ccode)]
distance$cname2 = panel$cname[match(distance$ccode2, panel$ccode)]
distance = distance[,c(1,11:12,4,8:10)]

# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
distance = distance[which(distance$cname2 %in% toKeep),]

# spread data
distance = distance %>% 
	gather(variable, value, -(ccode1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)
distance[is.na(distance)] = 0

# get p5 vars
distance$p5_capDistLogSum = apply(distance[,paste0(toKeep,'_capitalDistanceLog')], 1, sum)
distance$p5_centDistLogSum = apply(distance[,paste0(toKeep,'_centDistanceLog')], 1, sum)
distance$p5_minDistLogSum = apply(distance[,paste0(toKeep,'_minDistanceLog')], 1, sum)

# calc proportions
denom = rep(5,nrow(distance))
denom = ifelse(distance$cname1 %in% toKeep, 4, 5)
distance$p5_capDistLogAvg = distance$p5_capDistLogSum/denom
distance$p5_centDistLogAvg = distance$p5_centDistLogSum/denom
distance$p5_minDistLogAvg = distance$p5_minDistLogSum/denom
###############################################################

###############################################################
# Save to binaries
save(distance, file=paste0(
	pathData,'cshapes_distance/distance.rda')
)
###############################################################