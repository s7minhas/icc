if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

# subset to prelim state sample
prelimState = data[data$formal_icc_state!=1,]

# put back in one case where it went from prelim to formal in one year
add = data[which(data$prelim_icc_state==1 & data$formal_icc_state==1),]
prelimState = rbind(prelimState, add)

## prelim state
prelimStateVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind','lag1_pts',
	# 'lag1_osv_state',	
	'lag1_osv_state_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffMin'	 
	# 'lag1_p5int'
	# 'lag1_p5_latAngleMin'
	# 'lag1_p5_defAllyMin'
	)
prelimStateForm = formula(
	paste0('prelim_icc_state ~ ', 
		paste(prelimStateVars, collapse = ' + ')
		)
	)

# var transformations
prelimState$lag1_osv_state = log(prelimState$lag1_osv_state+1)
prelimState$lag1_osv_state_cumul = log(prelimState$lag1_osv_state_cumul+1)

# add splines
# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric() %>% flipBin()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# # Calculate
# prelimState$cSpline = with(prelimState,
# 	by(prelim_icc_state, cnameYear, function(y){
# 		getPeaceCounter(y) } ) ) %>% unlist()
# prelimState$cSpline2 = prelimState$cSpline^2
# prelimState$cSpline3 = prelimState$cSpline^3

# impute
loadPkg('sbgcop')
toImp = data.matrix(prelimState[,c('prelim_icc_state',prelimStateVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
prelimStateImp = data.frame(impData$Y.pmean)

mod = glm( prelimStateForm,
	family=binomial(link='logit'),
	data=prelimStateImp )
# sink(file=paste0(pathGraphics, 'prelimStateGLM.txt'))
summary(mod)
# sink()

loadPkg('Zelig')
z.out <- zelig(prelimStateForm, model = "relogit", 
	data = prelimStateImp)
sink(file=paste0(pathGraphics, 'prelimStateKing.txt'))
summary(z.out)
sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(prelimStateForm, data = prelimStateImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('Prelim State') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
ggsave(gg, file=paste0(pathGraphics, 'prelimStateStan.pdf'), width=10, height=5)