if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData.rda'))

# merge prelimState and prelimOpp
prelim = prelimOpp

# find cases in prelimState that are not in prelimOpp
miss = setdiff(prelimState$cnameYear, prelim$cnameYear)
rownames(prelimState)  = prelimState$cnameYear
prelim = rbind( prelim,  prelimState[miss,] )

# osv mods
prelim$lag1_osv = prelim$lag1_osv_state + prelim$lag1_osv_rebel
prelim$lag1_osv_cumul = prelim$lag1_osv_state_cumul + prelim$lag1_osv_rebel_cumul

osvVars = paste0('lag1_osv', 
	c('','_state','_rebel','_cumul','_state_cumul', '_rebel_cumul') )
for(v in osvVars){ prelim[,v] = log(prelim[,v] + 1) }

## prelim
prelimVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juhcind','lag1_pts',
	# osv vars:
	'lag1_osv_state', 'lag1_osv_rebel',
	# 'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul',
	# 'lag1_osv',	
	# 'lag1_osv_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffAvg'	 
	# 'lag1_p5_latAngleAvg'
	# 'lag1_p5_defAllyAvg'
	)
prelimForm = formula(
	paste0('prelim_icc ~ ', 
		paste(prelimVars, collapse = ' + ')
		)
	)

# add splines
# functions to help calculate peace years
flipBin = function(x,a=0,b=1){ z=x ; z[x==a]=b ; z[x==b]=a ; return(z) }
getPeaceCounter = function(x){
	tmp = x %>% as.numeric() %>% flipBin()
	peaceT = tmp * ave(tmp, c(0, cumsum(diff(tmp) != 0)), FUN = seq_along)
	return(peaceT) }

# Calculate
prelim$cSpline = with(prelim,
	by(prelim_icc, cnameYear, function(y){
		getPeaceCounter(y) } ) ) %>% unlist()
prelim$cSpline2 = prelim$cSpline^2
prelim$cSpline3 = prelim$cSpline^3

# impute
loadPkg('sbgcop')
toImp = data.matrix(prelim[,c('prelim_icc',prelimVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
prelimImp = data.frame(impData$Y.pmean)

mod = glm( prelimForm,
	family=binomial(link='logit'),
	data=prelimImp )
summary(mod)
# sink(file=paste0(pathGraphics, 'prelimGLM.txt'))
# summary(mod)
# sink()

loadPkg('Zelig')
z.out <- zelig(prelimForm, model = "relogit", 
	data = prelimImp)
summary(z.out)
# sink(file=paste0(pathGraphics, 'prelimKing.txt'))
# summary(z.out)
# sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(prelimForm, data = prelimImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('Prelim') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
gg
# ggsave(gg, file=paste0(pathGraphics, 'prelimStan.pdf'), width=10, height=5)