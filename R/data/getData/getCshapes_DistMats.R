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
# Save to binaries
save(distance, file=paste0(
	pathData,'cshapes_distance/distance.rda')
)
###############################################################