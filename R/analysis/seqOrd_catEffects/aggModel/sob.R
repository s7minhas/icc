###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_state_cumul',	
	'lag1_osv_rebel_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin'
	)

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)

# add revised icc stuff
icc = read_dta(paste0(pathData, 'icclevel_yearly_nojumps.dta'))
icc$cname = cname(icc$statenme)
icc$cname[icc$cname=='Yugoslavia'] = 'SERBIA'
icc$cnameYear = with(icc, paste(cname, year, sep='_'))
data$icc_nojumps = icc$icc_nojumps[match(data$cnameYear, icc$cnameYear)]
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'sobState_imp.rda'))){
	toImp = data.matrix(
		data[,c(
			'icclevel',
			sobVars
			)]
		)
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'sobState_imp.rda'))
} else { load(paste0(pathData, 'sobState_imp.rda')) }

# pick a few from the posterior
set.seed(6886)
frame = data.frame(impData$Y.pmean)
frame = cbind(data[,c('ccode','year','icc_nojumps')], frame)
frame$icc_nojumps = as.integer(frame$icc_nojumps + 1)
frame$ccode = as.integer(frame$ccode)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	x = cbind(data[,c('ccode','year','icc_nojumps')], x)
	names(x) = names(frame)
	x$icc_nojumps = as.integer(x$icc_nojumps + 1)
	x$ccode = as.integer(x$ccode)
	return(x) })
###############################################################

###############################################################
# create p5 variable (2, 365, 220, 710, 200)
frame$p5 = ifelse(
	frame$ccode %in% c(2, 365, 220, 710, 200), 0, 1 )

# # modify p5 var to be zero if p5 country
# frame$lag1_p5_absidealdiffMin = frame$p5*frame$lag1_p5_absidealdiffMin

# create summed osv var
frame$lag1_osv_cumul = frame$lag1_osv_rebel_cumul + frame$lag1_osv_state_cumul
###############################################################

###############################################################
# add random effect by case id
cntries = unique(frame$ccode)
id = 1
newFrame = NULL
for(ctry in cntries){
	slice = frame[frame$ccode == ctry,]
	iccChange = diff(slice$icc_nojumps)
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

frame = newFrame
frame$id = factor(frame$id)
###############################################################

###############################################################
# category specific effects
sobVars[c(5:8)] = paste0('cs(',sobVars[c(5:8)],')')

# pool
sobStateForm = formula(
	paste0('icc_nojumps ~ ', 
		paste(sobVars, collapse = ' + ') ) )
# mod = brm(
# 	formula=sobStateForm, 
# 	data=frame,
# 	family=cratio(link='logit')
# 	)
# save(mod, file=paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))

# hier
frame$icc_nojumps = factor(frame$icc_nojumps, ordered=TRUE)
sobStateForm = formula(
	paste0('icc_nojumps ~ ', 
		paste(sobVars, collapse = ' + '), '+(1|id)' ) )
# modHier = brm(
# 	formula=sobStateForm, 
# 	data=frame,
# 	family=cratio(link='logit'), 
# 	cores=4
# 	)
# summary(modHier)


f1 = formula(icc_nojumps ~ 
		icc_rat + lag1_civilwar + 
		lag1_polity2 + lag1_gdpCapLog + 
		cs(africa) +
		cs(lag1_osv_cumul) +
		cs(lag1_v2juncind) +
		cs(lag1_p5_absidealdiffMin) +
		(1|id))
modF1 = brm(
	formula=f1, 
	data=frame,
	family=cratio(link='logit'), 
	cores=4
	)
summary(modF1)

f2 = formula(icc_nojumps ~ 
		icc_rat + lag1_civilwar + 
		lag1_polity2 + lag1_gdpCapLog + 
		africa +
		cs(lag1_osv_cumul) +
		cs(lag1_v2juncind) + 
		cs(lag1_p5_absidealdiffMin) +		
		(1|id))
modF2 = brm(
	formula=f2, 
	data=frame,
	family=cratio(link='logit'), 
	cores=4
	)	
summary(modF2)

f3 = formula(icc_nojumps ~ 
		icc_rat + lag1_civilwar + 
		lag1_polity2 + lag1_gdpCapLog + 
		africa +
		lag1_osv_cumul +
		lag1_v2juncind + 
		lag1_p5_absidealdiffMin +		
		(1|id))
modF3 = brm(
	formula=f3, 
	data=frame,
	family=cratio(link='logit'), 
	cores=4
	)
summary(modF3)

# save(modHier, 
# 	file=paste0(
# 		pathResults, 'sobState_model1a_1_newp5Var_hier.rda'
# 		))
###############################################################