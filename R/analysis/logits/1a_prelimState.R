###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg('sbgcop')
###############################################################	

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

# subset to prelim state sample
prelimState = data[data$formal_icc_state!=1,]

# put back in one case where it went from prelim to formal in one year
add = data[which(data$prelim_icc_state==1 & data$formal_icc_state==1),]
prelimState = rbind(prelimState, add)
###############################################################

###############################################################
## prelim state
prelimStateVars = c(
	'icc_rat','lag1_civilwar',
	'lag1_polity2',
	'lag1_gdpCapLog',
	'africa',
	'lag1_v2juncind','lag1_pts',
	'lag1_osv_state_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyAvg',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
prelimState$lag1_osv_state_cumul = log(prelimState$lag1_osv_state_cumul+1)
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'prelimState_imp.rda'))){
	toImp = data.matrix(prelimState[,c('prelim_icc_state',prelimStateVars)])
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'prelimState_imp.rda'))
} else { load(paste0(pathData, 'prelimState_imp.rda')) }

# pick a few from the posterior
set.seed(6886)
frame = data.frame(impData$Y.pmean)
impDFs = lapply(sample(500:1000, 10), function(i){
	x = data.frame(impData$Y.impute[,,i])
	names(x) = names(frame); return(x) })
###############################################################

###############################################################
# specs
varSpecs = list(
	base = prelimStateVars[1:8],
	base_UN = prelimStateVars[1:9],
	base_Ally = prelimStateVars[c(1:8,10)],
	base_int = prelimStateVars[c(1:8,11:12)],
	base_UN_int = prelimStateVars[c(1:9,11:12)],
	base_Ally_int = prelimStateVars[c(1:8,10:12)],
	all = prelimStateVars )

formSpecs = lapply(varSpecs, function(x){
	formula( paste0('prelim_icc_state ~ ', 
			paste(x, collapse = ' + ') ) ) })

# run model w/o imputation
prelimStateMods = lapply(formSpecs, function(f){
	mod = glm( f,
		family=binomial(link='logit'),
		data=prelimState )	
	beta = coef(mod)	
	serror = sqrt(diag(vcov(mod)))		
	summ = data.frame(cbind(beta, serror))
	summ$z = summ$beta/summ$serror	
	return(summ) })

# clean table
lazyCleanVars = gsub('_','',prelimStateVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(prelimStateVars,lazyCleanVars,prelimStateMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(prelimStateVars)*2,length(prelimStateVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'prelimState.tex'))

# run model w/ imputation
prelimStateMods = lapply(formSpecs, function(f){
	mods = lapply(impDFs, function(df){
		mod = glm( f,
			family=binomial(link='logit'),
			data=df )	
		beta = coef(mod)	
		serror = sqrt(diag(vcov(mod)))		
		return(list(beta=beta, serror=serror)) })
	summ = rubinCoef(
		do.call('rbind', lapply(mods, function(x){x$'beta'})),
		do.call('rbind', lapply(mods, function(x){x$'serror'}))
		)
	summ = data.frame(cbind(t(summ$beta), t(summ$se)))
	names(summ) = c('beta', 'serror')
	summ$z = summ$beta/summ$serror	
	return(summ) })

# clean table
lazyCleanVars = gsub('_','',prelimStateVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(prelimStateVars,lazyCleanVars,prelimStateMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(prelimStateVars)*2,length(prelimStateVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'prelimState_imp.tex'))
###############################################################