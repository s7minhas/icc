###############################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/icc/R/setup.R') }

#
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
newVars = c(
'lag1_v2juhcind',
'lag1_p5_absidealdiffMax',
'lag1_p5_latAngleMin',
'lag1_p5_defAllyMax',
'lag1_p5_gov_clean',
'lag1_p5_reb_clean',
'lag1_pts'  
  )

## prelim state
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',	
  # p5 vars: 
  'lag1_p5_absidealdiffMin'
)

spec1 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMin'
  )

spec3 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMax'
  )

spec4 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_latAngleMin'
  )

spec5 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_defAllyMax'
  )

spec6 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_gov_clean'
  )

spec7 = c(
  'icclevel_state',
  'icc_rat', 'lag1_civilwar', 
  'lag1_polity2',
  'lag1_gdpCapLog',
  'africa',
  'lag1_v2juhcind',
  'lag1_osv_state_cumul',
  'lag1_p5_absidealdiffMin',
  'lag1_p5_defAllyMax',
  'lag1_p5_gov_clean'
  )
spec1=spec7

fNameHier
fName
###############################################################