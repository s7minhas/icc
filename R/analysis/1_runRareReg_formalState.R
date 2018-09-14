if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData.rda'))

## formal state
formalStateVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juhcind','lag1_poi_pts',
	'lag1_poi_osv_state',	
	'lag1_p5_absidealdiffMin'	
	)
formalStateForm = formula(
	paste0('formal_icc_state ~ ', 
		paste(formalStateVars, collapse = ' + ')
		)
	)

# var transformations
formalState$lag1_poi_osv_state = log(formalState$lag1_poi_osv_state+1)

# add splines
# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric() %>% flipBin()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# Calculate
formalState$cSpline = with(formalState,
	by(formal_icc_state, cnameYear, function(y){
		getPeaceCounter(y) } ) ) %>% unlist()
formalState$cSpline2 = formalState$cSpline^2
formalState$cSpline3 = formalState$cSpline^3

# impute
loadPkg('sbgcop')
toImp = data.matrix(formalState[,c('formal_icc_state',formalStateVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
formalStateImp = data.frame(impData$Y.pmean)

mod = glm( formalStateForm,
	family=binomial(link='logit'),
	data=formalStateImp )
sink(file=paste0(pathGraphics, 'formalStateGLM.txt'))
summary(mod)
sink()

loadPkg('Zelig')
z.out <- zelig(formalStateForm, model = "relogit", 
	data = formalStateImp)
sink(file=paste0(pathGraphics, 'formalStateKing.txt'))
summary(z.out)
sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(formalStateForm, data = formalStateImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('Formal State') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(gg, file=paste0(pathGraphics, 'formalStateStan.pdf'), width=10, height=5)