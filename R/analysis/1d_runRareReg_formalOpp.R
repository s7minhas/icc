if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData.rda'))

#
loadPkg('sbgcop')


## formal opp
formalOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juhcind',
	'lag1_poi_osv_rebel_cumul',	
	'lag1_p5_absidealdiffMin'	
	)
formalOppForm = formula(
	paste0('formal_icc_opp ~ ', 
		paste(formalOppVars, collapse = ' + ')
		)
	)

# var transformations
formalOpp$lag1_poi_osv_rebel = log(formalOpp$lag1_poi_osv_rebel+1)

# add splines
# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# Calculate
formalOpp$cSpline = with(formalOpp,
	by(formal_icc_opp, cnameYear, function(y){
		getPeaceCounter(y) } ) ) %>% unlist()
formalOpp$cSpline2 = formalOpp$cSpline^2
formalOpp$cSpline3 = formalOpp$cSpline^3

# impute
toImp = data.matrix(formalOpp[,c('formal_icc_opp',formalOppVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
formalOppImp = data.frame(impData$Y.pmean)

mod = glm( formalOppForm,
	family=binomial(link='logit'),
	data=formalOppImp )
summary(mod)
sink(file=paste0(pathGraphics, 'formalOppGLM.txt'))
summary(mod)
sink()

loadPkg('Zelig')
z.out <- zelig(formalOppForm, model = "relogit", 
	data = formalOppImp)
sink(file=paste0(pathGraphics, 'formalOppKing.txt'))
summary(z.out)
sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(formalOppForm, data = formalOppImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('Formal Opp') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(gg, file=paste0(pathGraphics, 'formalOppStan.pdf'), width=10, height=5)