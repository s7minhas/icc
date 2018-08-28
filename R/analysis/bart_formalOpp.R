if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

loadPkg('bartMachine')
load(paste0(pathData, 'mergedData.rda'))
pathResults = '~/Desktop/'
pathGraphics = '~/Desktop/'

## formal opp
formalOppVars = c(
	'icc_rat',
	'lag1_poi_osv_state', 
	'lag1_poi_osv_rebel',
	'lag1_poi_pts',
	'lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
formalOppForm = formula(
	paste0('formal_icc_state ~ ', 
		paste(formalOppVars, collapse = ' + ')
		)
	)
xMat = formalOpp[,formalOppVars]
y = formalOpp$formal_icc_opp

set.seed(6886)
bmFormalOpp = bartMachine(
	X = xMat,
	y = factor(y),
	num_trees=50,
	num_burn_in=250,
	num_iterations_after_burn_in=1000,
	alpha=.95, beta=2, k=2, q=.9, nu=3,
	use_missing_data=TRUE,
	serialize=TRUE
	)

#
pdf(file=paste0(pathGraphics, 'formalOpp_conv.pdf'))
plot_convergence_diagnostics(bmFormalOpp)
dev.off()

#
pdf(file=paste0(pathGraphics, 'formalOpp_varImp.pdf'))
investigate_var_importance(bmFormalOpp, 
	plot=TRUE,
	num_replicates_for_avg = 20
	)
dev.off()

# #
# cov_importance_test(bmFormalOpp)

# #
# var_selection_by_permute_response_cv(bmFormalOpp)

#
pdf(file=paste0(pathGraphics, 'formalOpp_intPlot.pdf'))
interaction_investigator(bmFormalOpp)
dev.off()

#
vars = c(
	'lag1_v2juhcind',
	'lag1_poi_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_civilwar',
	'lag1_polity2',	
	'lag1_osv_rebel'
	)
for(v in vars){
	pdf(file=paste0(pathGraphics, 'formalOpp_',v,'.pdf'))
	pd_plot(bmFormalOpp, j = v)
	dev.off()
}