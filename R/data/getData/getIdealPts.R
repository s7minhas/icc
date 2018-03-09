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
# Save
save(idPt, file=paste0(pathData, 'Voeten/idPt.rda'))
###############################################################