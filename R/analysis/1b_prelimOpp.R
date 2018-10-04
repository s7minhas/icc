###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg('sbgcop')
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

# subset to prelim state sample
prelimOpp = data[data$formal_icc_opp!=1,]

# put back in one case where it went from prelim to formal in one year
add = data[which(data$prelim_icc_opp==1 & data$formal_icc_opp==1),]
prelimOpp = rbind(prelimOpp, add)
###############################################################

###############################################################
## prelim opp
prelimOppVars = c(
	'icc_rat','lag1_civilwar',
	'lag1_polity2', 
	'lag1_gdpCapLog',
	'africa',
	'lag1_v2juncind',
	'lag1_osv_rebel_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyAvg',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
prelimOpp$lag1_osv_rebel_cumul = log(prelimOpp$lag1_osv_rebel_cumul+1)
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'prelimOpp_imp.rda'))){
	toImp = data.matrix(prelimOpp[,c('prelim_icc_opp',prelimOppVars)])
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'prelimOpp_imp.rda'))
} else { load(paste0(pathData, 'prelimOpp_imp.rda')) }

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
	base = prelimOppVars[1:7],
	base_UN = prelimOppVars[1:8],
	base_Ally = prelimOppVars[c(1:7,9)],
	base_int = prelimOppVars[c(1:7,10:11)],
	base_UN_int = prelimOppVars[c(1:8,10:11)],
	base_Ally_int = prelimOppVars[c(1:7,9:11)],
	all = prelimOppVars )

formSpecs = lapply(varSpecs, function(x){
	formula( paste0('prelim_icc_opp ~ ', 
			paste(x, collapse = ' + ') ) ) })

# run model w/o imputation
prelimOppMods = lapply(formSpecs, function(f){
	mod = glm( f,
		family=binomial(link='logit'),
		data=prelimOpp )	
	beta = coef(mod)	
	serror = sqrt(diag(vcov(mod)))		
	summ = data.frame(cbind(beta, serror))
	summ$z = summ$beta/summ$serror	
	return(summ) })

# clean table
lazyCleanVars = gsub('_','',prelimOppVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(prelimOppVars,lazyCleanVars,prelimOppMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(prelimOppVars)*2,length(prelimOppVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'prelimOpp.tex'))

# run model w/ imputation
prelimOppMods = lapply(formSpecs, function(f){
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
lazyCleanVars = gsub('_','',prelimOppVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(prelimOppVars,lazyCleanVars,prelimOppMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(prelimOppVars)*2,length(prelimOppVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'prelimOpp_imp.tex'))
###############################################################