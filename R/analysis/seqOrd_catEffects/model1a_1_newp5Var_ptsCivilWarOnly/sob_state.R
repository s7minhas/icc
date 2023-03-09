###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop', 'brms', 'future'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## prelim state
sobStateVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_state_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin'
	)

# var transformations
data$lag1_osv_state_cumul = log(data$lag1_osv_state_cumul+1)
###############################################################

###############################################################
# load imputed data
# sobOpp_imp_fin.rda is generated in:
	# ~/R/analysis/seqOrd_catEffects/model1_1_newp5Var/sob_state.R
load(paste0(pathData, 'sobState_imp_fin.rda'))

# reorg data for analysis
set.seed(6886)
frame = data.frame(impData$Y.pmean)
frame = cbind(data[,c('ccode','year','icclevel_state_3')], frame)
frame$icclevel_state_3 = as.integer(frame$icclevel_state_3 + 1)
frame$ccode = as.integer(frame$ccode)
frame$ccodeYear = with(frame, paste(ccode, year, sep='_'))

# yearly variables
yrlyVars = c(
  'lag1_polity2', 'lag1_gdpCapLog', 
  'lag1_v2juncind', 'lag1_p5_absidealdiffMin' )

# clean up imputed datasets
set.seed(6886) ; toPull = sample(500:1000, 10)
impDFs = lapply(toPull, function(i){

	# pull out an imputed dataset
	x = data.frame(impData$Y.impute[,,i])
	names(x) = colnames(impData$Y.pmean)

	# add back in id vars
	x = cbind(data[,c('ccode','year','icclevel_state_3')], x)

	# clean up ids
	x$ccode = as.integer(x$ccode)
	x$ccodeYear = with(x, paste(ccode, year, sep='_'))

	# merge from imputed data back to data object
	for(v in yrlyVars){
	data[,v] = x[match(data$ccodeYear,x$ccodeYear),v] }  

	# subset to relevant vars
	data = data[,c(
	'ccode','cname','year',
	'icclevel_state_3',
	sobStateVars )]

	# clean up and gen final vars
	x = data
	x$icclevel_state_3 = as.integer(x$icclevel_state_3 + 1)
	x$ccode = as.integer(x$ccode)  

	# create p5 variable (2, 365, 220, 710, 200)
	x$p5 = ifelse(
		x$ccode %in% c(2, 365, 220, 710, 200), 0, 1 )

	# modify p5 var to be zero if p5 country
	x$lag1_p5_absidealdiffMin = x$p5*x$lag1_p5_absidealdiffMin

	#
	return(x) })
###############################################################

###############################################################
# add random effect by case id
cntries = unique(frame$ccode)
id = 1
newFrame = NULL
for(ctry in cntries){
	slice = frame[frame$ccode == ctry,]
	slice = slice[order(slice$year),]
	iccChange = diff(slice$icclevel_opp_3)
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
# only incl states with civ war or pts>=3 since 2002
load(paste0(pathData, 'subset_ptsCivWar_cntries.rda'))

## keep only states in countries vector
impDFs = lapply(impDFs, function(x){
	out = x[which(x$ccode %in% cntries$ccode),]
	return(out) })

# remove civil war covariate
sobStateVars = sobStateVars[-which(sobStateVars %in% c('lag1_civilwar'))]
###############################################################

###############################################################
# category specific effects
sobStateVars[c(4:7)] = paste0('cs(',sobStateVars[c(4:7)],')')

# run model
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + ') ) )
plan(multiprocess)
mod = brm_multiple(
	formula=sobStateForm, 
	data=impDFs,
	family=cratio(link='logit')
	)
save(mod, 
	file=paste0(pathResults, 
		'sobState_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda'))
###############################################################