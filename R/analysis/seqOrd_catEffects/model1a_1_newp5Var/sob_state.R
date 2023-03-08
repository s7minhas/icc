###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
# yData = data
# load(paste0(pathData, 'mergedData_mnthly_ongoing.rda.rda'))

newVars = c(
'lag1_v2juhcind',
'lag1_p5_absidealdiffMax',
'lag1_p5_latAngleMin',
'lag1_p5_defAllyMax',
'lag1_p5_gov_clean',
'lag1_p5_reb_clean',
'lag1_pts'  
  )

## prelim state
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',	
  # p5 vars: 
  'lag1_p5_absidealdiffMin'
)

spec1 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMin'
  )

spec3 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMax'
  )

spec4 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_latAngleMin'
  )

spec5 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_defAllyMax'
  )

spec6 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_gov_clean'
  )

spec7 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMin',
  'lag1_p5_defAllyMax',
  'lag1_p5_gov_clean'
  )
spec1=spec7

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_state_cumul[is.na(data$lag1_osv_state_cumul)] = 0
###############################################################

###############################################################
# impute
# if(!file.exists(paste0(pathData, 'sobState_imp.rda'))){
toImp = data.matrix(data[,spec1])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
save(impData, file=paste0(pathData, 'sobState_impv2.rda'))
# } else { load(paste0(pathData, 'sobState_imp.rda')) }
# load(paste0(pathData, 'sobState_imp.rda'))

# pick a few from the posterior
set.seed(6886)
frame = data.frame(impData$Y.pmean)
frame = cbind(yData[,c('ccode','year','icclevel_state_3')], frame)
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
frame$ccode = as.integer(frame$ccode)
frame$ccodeYear = with(frame, paste(ccode, year, sep='_'))

yrlyVars = c(
  'lag1_polity2', 'lag1_gdpCapLog', 
  'lag1_v2juncind', 'lag1_p5_absidealdiffMin'
)
for(v in yrlyVars){
  data[,v] = frame[match(data$ccodeYear,frame$ccodeYear),v]
}

data = data[,c(
  'ccode','cname','year','date',
  'icclevel_state_3',
  sobStateVars
)]

frame = data
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
frame$ccode = as.integer(frame$ccode)
###############################################################

###############################################################
# create p5 variable (2, 365, 220, 710, 200)
frame$p5 = ifelse(
  frame$ccode %in% c(2, 365, 220, 710, 200), 0, 1 )

# modify p5 var to be zero if p5 country
frame$lag1_p5_absidealdiffMin = frame$p5*frame$lag1_p5_absidealdiffMin
###############################################################

###############################################################
# add random effect by case id
cntries = unique(frame$ccode)
id = 1
newFrame = NULL
cntries = cntries[-c(90,136)]
for(ctry in cntries){
  slice = frame[frame$ccode == ctry,]
  slice = slice[order(slice$date),]
  iccChange = diff(slice$icclevel_state_3)
  slice$id = id
  if(length(which(iccChange<0))>=1){
    id = id + 1
    newCase = (which(iccChange<0)+1):nrow(slice)
    if(length(which(iccChange<0))>1){ stop('hi') }
    slice$id[newCase] = id
  }
  id = id + 1
  newFrame = rbind(newFrame, slice)
}

# fix congo drc
slice = frame[frame$ccode %in% c(490,666),]
slice = frame[frame$ccode == 490,]
slice$id = 200
slice = slice[order(slice$date),]
slice$id[slice$date > as.Date('2005-02-01')] = 201
slice$id[slice$date > as.Date('2012-12-01')] = 202
newFrame = rbind(newFrame, slice)

# fix israel
slice = frame[frame$ccode == 666,]
slice$id = 203
slice = slice[order(slice$date),]
slice$id[slice$date > as.Date('2012-04-01')] = 204
slice$id[slice$date > as.Date('2014-11-01')] = 205
newFrame = rbind(newFrame, slice)

frame = newFrame
frame$id = factor(frame$id)
###############################################################

###############################################################
# category specific effects
sobStateVars[c(5:8)] = paste0('cs(',sobStateVars[c(5:8)],')')

# pool
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + ') ) )
# mod = brm(
# 	formula=sobStateForm, 
# 	data=frame,
# 	family=cratio(link='logit'),
# 	cores=4
# 	)
# # save(mod, file=paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
# summary(mod)

# hier
frame$icclevel_state_3 = factor(frame$icclevel_state_3, ordered=TRUE)
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + '), '+(1|id)' ) )
modHier = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit'), 
	cores=4
	)
summary(modHier)
# save(modHier, 
# 	file=paste0(
# 		pathResults, 'sobState_model1a_1_newp5Var_hier.rda'
# 		))
###############################################################