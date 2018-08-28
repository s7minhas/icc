if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/icc/R/setup.R') }

loadPkg('bartMachine')
load(paste0(pathData, 'mergedData.rda'))
pathResults = '~/Desktop/'
pathGraphics = '~/Desktop/'

## prelim opp
prelimOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','lag1_popLog','africa',
	'lag1_v2juhcind','lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_osv_rebel','lag1_osv_state',
	'lag1_p5_intv_rebel_any',
	'lag1_p5_intv_state_any'	
	)
prelimOppForm = formula(
	paste0('prelim_icc_state ~ ', 
		paste(prelimOppVars, collapse = ' + ')
		)
	)
xMat = prelimOpp[,prelimOppVars]
y = prelimOpp$prelim_icc_opp

set.seed(6886)
bmPrelimOpp = bartMachine(
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
pdf(file=paste0(pathGraphics, 'prelimOpp_conv.pdf'))
plot_convergence_diagnostics(bmPrelimOpp)
dev.off()

#
pdf(file=paste0(pathGraphics, 'prelimOpp_varImp.pdf'))
investigate_var_importance(bmPrelimOpp, 
	plot=TRUE,
	num_replicates_for_avg = 20
	)
dev.off()

# #
# cov_importance_test(bmPrelimOpp)

# #
# var_selection_by_permute_response_cv(bmPrelimOpp)

#
pdf(file=paste0(pathGraphics, 'prelimOpp_intPlot.pdf'))
interaction_investigator(bmPrelimOpp)
dev.off()

#
vars = c(
	'lag1_v2juhcind',
	'lag1_pts',
	'lag1_p5_absidealdiffAvg',
	'lag1_civilwar',
	'lag1_polity2',	
	'lag1_osv_rebel'
	)
for(v in vars){
	pdf(file=paste0(pathGraphics, 'prelimOpp_',v,'.pdf'))
	pd_plot(bmPrelimOpp, j = v)
	dev.off()
}