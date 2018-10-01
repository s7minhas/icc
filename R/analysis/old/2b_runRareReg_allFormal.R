if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

load(paste0(pathData, 'mergedData.rda'))

# sampling frame
cYears = unique(c(formalOpp$cnameYear, formalState$cnameYear))
formal = rbind(formalState, formalOpp)
formal = unique(formal[,c(1:5,15:ncol(formal))])

# add back in formal icc measures
formal$formal_icc_state = formalState$formal_icc_state[match(formal$cnameYear, formalState$cnameYear)]
formal$formal_icc_state[is.na(formal$formal_icc_state)] = 0
formal$formal_icc_opp = formalOpp$formal_icc_opp[match(formal$cnameYear, formalOpp$cnameYear)]
formal$formal_icc_opp[is.na(formal$formal_icc_opp)] = 0
formal$formal_icc = formal$formal_icc_state + formal$formal_icc_opp
formal$formal_icc = ifelse(formal$formal_icc>1, 1, 0)

# osv mods
formal$lag1_osv = formal$lag1_osv_state + formal$lag1_osv_rebel
formal$lag1_osv_cumul = formal$lag1_osv_state_cumul + formal$lag1_osv_rebel_cumul
formal$lag1_poi_osv = formal$lag1_poi_osv_state + formal$lag1_poi_osv_rebel

osvVars = paste0('lag1_osv', 
	c('','_state','_rebel','_cumul','_state_cumul', '_rebel_cumul') )
osvVars = c(osvVars, 
	paste0('lag1_poi_osv', 
		c('', '_state', '_rebel') ) )
for(v in osvVars){ formal[,v] = log(formal[,v] + 1) }

## formal
formalVars = c(
	# 'icc_rat',
	'lag1_polity2',
	'lag1_gdpCapLog','africa',
	# 'lag1_v2juhcind','lag1_pts',
	'lag1_v2juhcind',
	# 'lag1_poi_pts',
	# osv vars:
	# 'lag1_poi_osv_state', 'lag1_poi_osv_rebel',
	'lag1_poi_osv',
	# 'lag1_osv_state', 'lag1_osv_rebel',
	# 'lag1_osv_state_cumul', 'lag1_osv_rebel_cumul',
	# 'lag1_osv',	
	# 'lag1_osv_cumul',	
	# p5 vars: 
	'lag1_p5_absidealdiffAvg'	 
	# 'lag1_p5_latAngleAvg'
	# 'lag1_p5_defAllyAvg'
	)
formalForm = formula(
	paste0('formal_icc ~ ', 
		paste(formalVars, collapse = ' + ')
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
formal$cSpline = with(formal,
	by(formal_icc, cnameYear, function(y){
		getPeaceCounter(y) } ) ) %>% unlist()
formal$cSpline2 = formal$cSpline^2
formal$cSpline3 = formal$cSpline^3

# impute
loadPkg('sbgcop')
toImp = data.matrix(formal[,c('formal_icc',formalVars)])
impData = sbgcop.mcmc(Y=toImp, seed=6886, verb=FALSE)
formalImp = data.frame(impData$Y.pmean)

mod = glm( formalForm,
	family=binomial(link='logit'),
	data=formalImp )
summary(mod)
# sink(file=paste0(pathGraphics, 'formalGLM.txt'))
# summary(mod)
# sink()

loadPkg('Zelig')
z.out <- zelig(formalForm, model = "relogit", 
	data = formalImp)
summary(z.out)
# sink(file=paste0(pathGraphics, 'formalKing.txt'))
# summary(z.out)
# sink()

loadPkg('rstanarm')
t_prior = student_t(df = 7, location = 0, scale = 2.5)
fit1 = stan_glm(formalForm, data = formalImp, 
	family = binomial(link = "logit"), 
	prior = t_prior, prior_intercept = t_prior,
	chains = 2, cores = 2, seed = 6886, iter = 1000)
round(fit1$stan_summary[,c('2.5%','10%','mean', '90%','97.5%')],2)

gg = plot(fit1) +
	ggtitle('formal') +
	geom_vline(aes(xintercept=0), linetype=2) +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
		)
gg
# ggsave(gg, file=paste0(pathGraphics, 'formalStan.pdf'), width=10, height=5)