###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','rstanarm'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

# subset to states that have experienced a prelim
formalOpp = data[data$prelim_icc_opp==1 | data$formal_icc_opp==1,]
###############################################################

###############################################################
## formal opp
formalOppVars = c(
	'icc_rat','lag1_civilwar',
	'lag1_polity2',
	'lag1_gdpCapLog',
	'africa',
	'lag1_v2juncind',
	'lag1_poi_osv_rebel',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin',
	'lag1_p5_defAllyAvg',
	'lag1_p5_gov_clean', 'lag1_p5_reb_clean'
	)

# var transformations
formalOpp$lag1_poi_osv_rebel = log(formalOpp$lag1_poi_osv_rebel+1)
###############################################################

###############################################################
# impute
if(!file.exists(paste0(pathData, 'formalOpp_imp.rda'))){
	toImp = data.matrix(formalOpp[,c('formal_icc_opp',formalOppVars)])
	impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE, nsamp=1000)
	save(impData, file=paste0(pathData, 'formalOpp_imp.rda'))
} else { load(paste0(pathData, 'formalOpp_imp.rda')) }

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
	base = formalOppVars[1:7],
	base_UN = formalOppVars[1:8],
	base_Ally = formalOppVars[c(1:7,9)],
	base_int = formalOppVars[c(1:7,10:11)],
	base_UN_int = formalOppVars[c(1:8,10:11)],
	base_Ally_int = formalOppVars[c(1:7,9:11)],
	all = formalOppVars )

formSpecs = lapply(varSpecs, function(x){
	formula( paste0('formal_icc_opp ~ ', 
			paste(x, collapse = ' + ') ) ) })

# b model w/o imputation
t_prior = student_t(df = 7, location = 0, scale = 2.5)
formalOppMods = lapply(formSpecs, function(f){
	mod = stan_glm(f, data = formalOpp, 
		family = binomial(link = "logit"), 
		prior = t_prior, prior_intercept = t_prior,
		chains = 4, cores = 2, seed = 6886, iter = 1000)
	beta = mod$coefficients
	serror = mod$ses
	summ = data.frame(cbind(beta, serror))
	summ$z = summ$beta/summ$serror	
	return(summ) })

# # run model w/o imputation
# formalOppMods = lapply(formSpecs, function(f){
# 	mod = glm( f,
# 		family=binomial(link='logit'),
# 		data=formalOpp )	
# 	beta = coef(mod)	
# 	serror = sqrt(diag(vcov(mod)))		
# 	summ = data.frame(cbind(beta, serror))
# 	summ$z = summ$beta/summ$serror	
# 	return(summ) })

# clean table
lazyCleanVars = gsub('_','',formalOppVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(formalOppVars,lazyCleanVars,formalOppMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(formalOppVars)*2,length(formalOppVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'formalOpp.tex'))

# b model w/ imputation
t_prior = student_t(df = 7, location = 0, scale = 2.5)
formalOppMods = lapply(formSpecs, function(f){
	mods = lapply(impDFs, function(df){
		mod = stan_glm(f, data = df, 
			family = binomial(link = "logit"), 
			prior = t_prior, prior_intercept = t_prior,
			chains = 4, cores = 2, seed = 6886, iter = 1000)
		beta = mod$coefficients
		serror = mod$ses
		return(list(beta=beta, serror=serror)) })
	summ = rubinCoef(
		do.call('rbind', lapply(mods, function(x){x$'beta'})),
		do.call('rbind', lapply(mods, function(x){x$'serror'}))
		)
	summ = data.frame(cbind(t(summ$beta), t(summ$se)))
	names(summ) = c('beta', 'serror')
	summ$z = summ$beta/summ$serror	
	return(summ) })

# # run model w/ imputation
# formalOppMods = lapply(formSpecs, function(f){
# 	mods = lapply(impDFs, function(df){
# 		mod = glm( f,
# 			family=binomial(link='logit'),
# 			data=df )	
# 		beta = coef(mod)	
# 		serror = sqrt(diag(vcov(mod)))		
# 		return(list(beta=beta, serror=serror)) })
# 	summ = rubinCoef(
# 		do.call('rbind', lapply(mods, function(x){x$'beta'})),
# 		do.call('rbind', lapply(mods, function(x){x$'serror'}))
# 		)
# 	summ = data.frame(cbind(t(summ$beta), t(summ$se)))
# 	names(summ) = c('beta', 'serror')
# 	summ$z = summ$beta/summ$serror	
# 	return(summ) })

# clean table
lazyCleanVars = gsub('_','',formalOppVars,fixed=TRUE)
lazyCleanMods = gsub('_',' ',names(varSpecs),fixed=TRUE)
lab='$^{**}$ and $^{*}$ indicate significance at $p< 0.05 $ and $p< 0.10 $, respectively.'
res=getTable(formalOppVars,lazyCleanVars,formalOppMods,lazyCleanMods)
print.xtable(xtable(res, align='llccccccc', caption=lab),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,length(formalOppVars)*2,length(formalOppVars)*2),
	size="footnotesize",	
	file=paste0(pathResults, 'formalOpp_imp.tex'))
###############################################################