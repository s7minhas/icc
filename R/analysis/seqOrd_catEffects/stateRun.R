###############################################################
# load data

###############################################################

###############################################################
# category specific effects
sobStateVars[c(5:8)] = paste0('cs(',sobStateVars[c(5:8)],')')

# pool
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + ') ) )
# mod = brm(
# 	formula=sobStateForm, 
# 	data=frame,
# 	family=cratio(link='logit'),
# 	cores=4
# 	)
# # save(mod, file=paste0(pathResults, 'sobState_model1a_1_newp5Var.rda'))
# summary(mod)

# hier
frame$icclevel_state_3 = factor(frame$icclevel_state_3, ordered=TRUE)
sobStateForm = formula(
	paste0('icclevel_state_3 ~ ', 
		paste(sobStateVars, collapse = ' + '), '+(1|id)' ) )
modHier = brm(
	formula=sobStateForm, 
	data=frame,
	family=cratio(link='logit'), 
	cores=4
	)
summary(modHier)
# save(modHier, 
# 	file=paste0(
# 		pathResults, 'sobState_model1a_1_newp5Var_hier.rda'
# 		))
###############################################################