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
idPt[is.na(idPt)] = NA

# org for calculation
pData = lapply(toKeep, function(v){
	slice = idPt[which(idPt$cname1 == v),]
	slice[,which(grepl(v, names(idPt)))] = NA
	return(slice) })
pData[[length(pData) + 1 ]] = idPt[which(!idPt$cname1 %in% toKeep),]

# loop over each using p5Vars function
pData = lapply(pData, function(x){
	p5Vars(x, paste0(toKeep,'_absidealdiff'), 'absidealdiff') })

# reorg
idPt = do.call('rbind', pData)
###############################################################

###############################################################
# Save
names(idPt)[1] = 'ccode'
save(idPt, file=paste0(pathData, 'Voeten/idPt.rda'))
###############################################################