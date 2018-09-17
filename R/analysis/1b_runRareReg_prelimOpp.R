if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData.rda'))

## prelim opp
prelimOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juhcind',
	'lag1_osv_rebel',	
	'lag1_p5_absidealdiffMin'	
	)
prelimOppForm = formula(
	paste0('prelim_icc_opp ~ ', 
		paste(prelimOppVars, collapse = ' + ')
		)
	)

# var transformations
prelimOpp$lag1_osv_rebel = log(prelimOpp$lag1_osv_rebel+1)

# add splines
# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric() %>% flipBin()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# Calculate
prelimOpp$cSpline = with(prelimOpp,
	by(prelim_icc_opp, cnameYear, function(y){
		getPeaceCounter(y) } ) ) %>% unlist()
prelimOpp$cSpline2 = prelimOpp$cSpline^2
prelimOpp$cSpline3 = prelimOpp$cSpline^3

# impute
loadPkg('sbgcop')
toImp = data.matrix(prelimOpp[,c('prelim_icc_opp',prelimOppVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
prelimOppImp = data.frame(impData$Y.pmean)

mod = glm( prelimOppForm,
	family=binomial(link='logit'),
	data=prelimOppImp )
sink(file=paste0(pathGraphics, 'prelimOppGLM.txt'))
summary(mod)
sink()

loadPkg('Zelig')
z.out <- zelig(prelimOppForm, model = "relogit", 
	data = prelimOppImp)
sink(file=paste0(pathGraphics, 'prelimOppKing.txt'))
summary(z.out)
sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(prelimOppForm, data = prelimOppImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('Prelim Opp') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(gg, file=paste0(pathGraphics, 'prelimOppStan.pdf'), width=10, height=5)