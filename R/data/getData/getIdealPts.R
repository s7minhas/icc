####
if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/icc/R/setup.R') }
####

###############################################################
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
load(paste0(pathData,'Voeten/Dyadicdata.rdata')); idPt=x ; rm(list='x')
###############################################################

###############################################################
# Match idPt names to panel
cntries = unique(c(idPt$ccode2, idPt$ccode)) %>% data.frame(cowcode=.,stringsAsFactors = FALSE)
cntries$cname = panel$cname[match(cntries$cowcode,panel$COWCODE)]
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to idPt
idPt$cname1 = cntries$cname[match(idPt$ccode1, cntries$cowcode)]
idPt$cname2 = cntries$cname[match(idPt$ccode2, cntries$cowcode)]
idPt$ccode1 = cntries$ccode[match(idPt$ccode1, cntries$cowcode)]
idPt$ccode2 = cntries$ccode[match(idPt$ccode2, cntries$cowcode)]

# Check for duplicates
idPt$dyadidyr = paste(idPt$ccode1, idPt$ccode2, idPt$year, sep='_')
stopifnot( length( table(idPt$dyadidyr)[table(idPt$dyadidyr)>1] ) == 0 )
###############################################################

###############################################################
# match time frame
idPt = idPt[idPt$year>=2000,]

# relabel vars
idPt = idPt[,c(2,23:24,3,6,8,10)]

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
idPt$p5_s3unSum = apply(idPt[,paste0(toKeep,'_s3un')], 1, sum)
idPt$p5_agree3unSum = apply(idPt[,paste0(toKeep,'_agree3un')], 1, sum)

# calc proportions
denom = rep(5,nrow(idPt))
denom = ifelse(idPt$cname1 %in% toKeep, 4, 5)
idPt$p5_absidealdiffProp = idPt$p5_absidealdiffSum/denom
idPt$p5_s3unProp = idPt$p5_s3unSum/denom
idPt$p5_agree3unProp = idPt$p5_agree3unSum/denom
###############################################################

###############################################################
# Save
save(idPt, file=paste0(pathData, 'Voeten/idPt.rda'))
###############################################################