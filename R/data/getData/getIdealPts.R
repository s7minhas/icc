####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

###############################################################
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
# load(paste0(pathData,'Voeten/Dyadicdata.rdata')); idPt=x ; rm(list='x')	
load(paste0(pathData, 'Voeten/IdealpointsPublished.rdata')); idPtM =x ; rm(x)
idPtM = idPtM[which(idPtM$year>=1999),c('ccode','CountryName','year','Idealpoint')]
idPtM$cname = cname(idPtM$CountryName)
idPtM$cname[idPtM$cname=='Yugoslavia'] = 'SERBIA'
idPtM$ccode = panel$ccode[match(idPtM$cname,panel$cname)]
idPtM$cyear = paste(idPtM$cname, idPtM$year, sep='_')

idPt = lapply(unique(idPtM$year), function(yr){
	slice = idPtM[idPtM$year==yr,]
	cntries = unique(slice$cname)
	dFrame = expand.grid(cntries, cntries)
	dFrame = dFrame[dFrame$Var1!=dFrame$Var2,]
	dFrame$year = yr
	dFrame$cyear1 = paste(dFrame$Var1, dFrame$year, sep='_')
	dFrame$cyear2 = paste(dFrame$Var2, dFrame$year, sep='_')
	dFrame$idPt1 = idPtM$Idealpoint[match(dFrame$cyear1,idPtM$cyear)]
	dFrame$idPt2 = idPtM$Idealpoint[match(dFrame$cyear2,idPtM$cyear)]
	dFrame$absidealdiff = with(dFrame, abs(idPt1-idPt2))
	names(dFrame)[1:2]=c('cname1','cname2')
	dFrame$ccode1 = panel$ccode[match(dFrame$cname1,panel$cname)]
	dFrame$ccode2 = panel$ccode[match(dFrame$cname2,panel$cname)]
	out = dFrame[,c('ccode1','cname1','cname2','year','absidealdiff')]
	return(out) }) %>% do.call('rbind', .)

# Check for duplicates
dyadidyr = paste(idPt$ccode1, idPt$ccode2, idPt$year, sep='_')
stopifnot( length( table(dyadidyr)[table(dyadidyr)>1] ) == 0 )
###############################################################

###############################################################
# match time frame
idPt = idPt[idPt$year>=1999,]

# focus on p5 countries
toKeep = c(
	"UNITED STATES", "UNITED KINGDOM", 
	"CHINA", "RUSSIAN FEDERATION", "FRANCE")
idPt = idPt[which(idPt$cname2 %in% toKeep),]

# spread data
idPt = idPt %>% 
	gather(variable, value, -(ccode1:year)) %>%
	unite(temp, cname2, variable) %>%
	spread(temp, value)
idPt[is.na(idPt)] = 0

# get p5 vars
idPt$p5_absidealdiffSum = apply(idPt[,paste0(toKeep,'_absidealdiff')], 1, sum)

# calc proportions
denom = rep(5,nrow(idPt))
denom = ifelse(idPt$cname1 %in% toKeep, 4, 5)
idPt$p5_absidealdiffAvg = idPt$p5_absidealdiffSum/denom

# min and max
idPt$p5_absidealdiffMin = apply(idPt[,names(idPt)[4:8]], 1, min)
idPt$p5_absidealdiffMax = apply(idPt[,names(idPt)[4:8]], 1, max)
###############################################################

###############################################################
# Save
names(idPt)[1] = 'ccode'
save(idPt, file=paste0(pathData, 'Voeten/idPt.rda'))
###############################################################