###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

if(Sys.info()['user'] %in% c('herme', 'Owner', 'S7M')){
	user=Sys.info()['user']
	source(paste0('C:/Users/',user,'/Research/icc/R/setup.R')) }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))
# yData = data
# load(paste0(pathData, 'mergedData_mnthly_ongoing.rda.rda'))

## prelim state
sobOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_rebel_cumul',
	# p5 vars:
	'lag1_p5_absidealdiffMin'
	)

# var transformations
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)
# data$lag1_osv_rebel_cumul[data$cname=='UNITED STATES'] = 0
data$lag1_osv_rebel_cumul[is.na(data$lag1_osv_rebel_cumul)] = 0
###############################################################

# ###########################
# ## descriptive info for res design portion
# cntsState=table(data$icclevel_state_3)
# totState=sum(cntsState)
# propState = cntsState/totState
#
# cntsOpp=table(data$icclevel_opp_3)
# totOpp=sum(cntsOpp)
# propOpp = cntsOpp/totOpp
#
# cntsState
# round(propState, 2)
#
# cntsOpp
# round(propOpp, 2)
#
# sobStateVars = c(
#   'icc_rat','lag1_civilwar','lag1_polity2',
#   'lag1_gdpCapLog','africa',
#   'lag1_v2juncind',
#   'lag1_osv_state_cumul',
#   # p5 vars:
#   'lag1_p5_absidealdiffMin'
# )
#
# ivs = unique(c(sobOppVars, sobStateVars))
# dvs = c('icclevel_state_3', 'icclevel_opp_3')
# vars = c(ivs, dvs)
#
# dim(data)
# data = na.omit(data[,vars])
# dim(data)
#
# cntsState=table(data$icclevel_state_3)
# totState=sum(cntsState)
# propState = cntsState/totState
#
# cntsOpp=table(data$icclevel_opp_3)
# totOpp=sum(cntsOpp)
# propOpp = cntsOpp/totOpp
#
# cntsState
# round(propState, 2)
#
# cntsOpp
# round(propOpp, 2)
# ###########################

###############################################################
# # impute yearly level data
# # if(!file.exists(paste0(pathData, 'sobOpp_imp.rda'))){
# 	# toImp = data.matrix(data[,c('icclevel_opp',sobOppVars)])
# 	# impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
# 	# save(impData, file=paste0(pathData, 'sobOpp_imp.rda'))
# # } else { load(paste0(pathData, 'sobOpp_imp.rda')) }
# load(paste0(pathData, 'sobOpp_imp.rda'))
#
# # pick a few from the posterior
# set.seed(6886)
# frame = data.frame(impData$Y.pmean)
# frame = cbind(yData[,c('ccode','year','icclevel_opp_3')], frame)
# frame$icclevel_opp_3 = as.integer(frame$icclevel_opp_3 + 1)
# frame$ccode = as.integer(frame$ccode)
# frame$ccodeYear = with(frame, paste(ccode, year, sep='_'))
# # impDFs = lapply(sample(500:1000, 10), function(i){
# # 	x = data.frame(impData$Y.impute[,,i])
# # 	x = cbind(data[,c('ccode','year','icclevel_opp_3')], x)
# # 	names(x) = names(frame)
# # 	x$icclevel_opp_3 = as.integer(x$icclevel_opp_3 + 1)
# # 	x$ccode = as.integer(x$ccode)
# # 	return(x) })
#
# #
# yrlyVars = c(
# 	'lag1_polity2', 'lag1_gdpCapLog',
# 	'lag1_v2juncind', 'lag1_p5_absidealdiffMin'
# 	)
# for(v in yrlyVars){
# 	data[,v] = frame[match(data$ccodeYear,frame$ccodeYear),v]
# }

data = data[,c(
	'ccode','cname','year',
	# 'date',
	'icclevel_opp_3',
	sobOppVars
	)]

frame = na.omit(data)
frame$icclevel_opp_3 = as.integer(frame$icclevel_opp_3 + 1)
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
# cntries = unique(frame$ccode)
# id = 1
# newFrame = NULL
# for(ctry in cntries){
# 	slice = frame[frame$ccode == ctry,]
# 	slice = slice[order(slice$date),]
# 	iccChange = diff(slice$icclevel_opp_3)
# 	slice$id = id
# 	if(length(which(iccChange<0))>=1){
# 		id = id + 1
# 		newCase = (which(iccChange<0)+1):nrow(slice)
# 		if(length(which(iccChange<0))>1){ stop('hi') }
# 		slice$id[newCase] = id
# 	}
# 	id = id + 1
# 	newFrame = rbind(newFrame, slice)
# }
#
# frame = newFrame
# frame$id = factor(frame$id)
###############################################################

###############################################################
# category specific effects
sobOppVars[c(5:8)] = paste0('cs(',sobOppVars[c(5:8)],')')

# pool
sobOppForm = formula(
	paste0('icclevel_opp_3 ~ ',
		paste(sobOppVars, collapse = ' + ') ) )
mod = brm(
	formula=sobOppForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
	)
save(mod, file=paste0(pathResults, 'sobOpp_model1a_1_newp5Var_noImp.rda'))

summary(mod)

# hier
frame$icclevel_opp_3 = factor(frame$icclevel_opp_3, ordered=TRUE)
sobOppForm = formula(
	paste0('icclevel_opp_3 ~ ',
		paste(sobOppVars, collapse = ' + '), '+(1|id)' ) )
modHier = brm(
	formula=sobOppForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
	)
# save(modHier,
# 	file=paste0(
# 		pathResults, 'sobOpp_model1a_1_newp5Var_hier.rda'
# 		))
###############################################################
load(paste0(pathResults, 'sobOpp_model1a_1_newp5Var_hier.rda'))
names(modHier)
modHier$formula
modHier$algorithm

x = make_stancode(
	formula=sobOppForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4,
	save_model='C:/Users/herme/OneDrive/Desktop/tst.txt'
)
x

get_prior(
	formula=sobOppForm,
	data=frame,
	family=cratio(link='logit'),
	cores=4
)

prior_summary(modHier)

draws = rstudent_t(1000, 3, 0, 10)

plot(density(draws))
