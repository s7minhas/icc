###############################################################
source('~/Research/icc/R/setup.R')
loadPkg(c('sbgcop','brms'))
###############################################################

###############################################################
load(paste0(pathData, 'mergedData_yrly_ongoing.rda.rda'))

## descriptive info for res design portion
sobOppVars = c(
	'icc_rat','lag1_civilwar','lag1_polity2',
	'lag1_gdpCapLog','africa',
	'lag1_v2juncind',
	'lag1_osv_rebel_cumul',
	# p5 vars:
	'lag1_p5_absidealdiffMin' )

# var transformations
data$lag1_osv_rebel_cumul = log(data$lag1_osv_rebel_cumul+1)
data$lag1_osv_rebel_cumul[is.na(data$lag1_osv_rebel_cumul)] = 0

cntsState=table(data$icclevel_state_3)
totState=sum(cntsState)
propState = cntsState/totState

cntsOpp=table(data$icclevel_opp_3)
totOpp=sum(cntsOpp)
propOpp = cntsOpp/totOpp

cntsState
# round(propState, 2)

cntsOpp
# round(propOpp, 2)

#
sobStateVars = c(
  'icc_rat','lag1_civilwar','lag1_polity2',
  'lag1_gdpCapLog','africa',
  'lag1_v2juncind',
  'lag1_osv_state_cumul',
  # p5 vars:
  'lag1_p5_absidealdiffMin'
)

ids = c('ccode','cname','year')
ivs = unique(c(sobOppVars, sobStateVars))
dvs = c('icclevel_state_3', 'icclevel_opp_3')
vars = c(ids, ivs, dvs)

# summary(data[,vars])
#
# apply(data[,vars], 2, function(x){
# 	sum(is.na(x))/length(x)
# }) %>% cbind() %>% round(., 2)
#
# dim(data)
data = na.omit(data[,vars])
# dim(data)
# 2906-2336
# 570/2906

cntsState=table(data$icclevel_state_3)
totState=sum(cntsState)
propState = cntsState/totState

cntsOpp=table(data$icclevel_opp_3)
totOpp=sum(cntsOpp)
propOpp = cntsOpp/totOpp

print('listwise deletion')
cntsState
# round(propState, 2)

cntsOpp
# round(propOpp, 2)
(90-77)/90
(77-58)/77
###############################################################

###############################################################
# variation in length of time attrib to invest
slice = data[,c(
  'cname', 'year', 
  'icclevel_state_3', 'icclevel_opp_3' )]
rownames(slice) = NULL

# count up times of investigations
cntries = unique(slice$cname)

# remove countries that dont experience any
aggState = slice %>% group_by(cname) %>%
  summarize(state=sum(icclevel_state_3)) %>%
  data.frame(.,stringsAsFactors = F)
aggState = aggState[aggState$state>0,]
cntries_state = aggState$cname

aggOpp = slice %>% group_by(cname) %>%
  summarize(opp=sum(icclevel_opp_3)) %>%
  data.frame(.,stringsAsFactors = F)
aggOpp = aggOpp[aggOpp$opp>0,]
cntries_opp = aggOpp$cname

# for identified countries find time state cases
state = slice[slice$cname %in% cntries_state,c(1:3)]
stateTimes = lapply(cntries_state, function(x){

  # subset to relevant data
  tmp = state[state$cname==x,]
  tmp = tmp[order(tmp$year),]

  # set up counters
  prelimTime = 0
  formalTime = 0

  # iterate through and start
  # counting when seeing
  # repeating 1s and repeating 2s
  for(ii in 1:nrow(tmp)){

    if(tmp$icclevel_state_3[ii]==1){
      prelimTime = prelimTime + 1
    } else if(tmp$icclevel_state_3[ii]==2){
      formalTime = formalTime + 1
    }
  }

  # organize
  out = data.frame(cntry=x, stringsAsFactors = F)
  out$prelimTime = prelimTime
  out$formalTime = formalTime

  #
  return(out)
})
stateTimes = do.call(rbind, stateTimes)

# do the same for opp cases
opp = slice[slice$cname %in% cntries_opp,c(1:2,4)]
oppTimes = lapply(cntries_opp, function(x){

  # subset to relevant data
  tmp = opp[opp$cname==x,]
  tmp = tmp[order(tmp$year),]

  # set up counters
  prelimTime = 0
  formalTime = 0

  # iterate through and start
  # counting when seeing
  # repeating 1s and repeating 2s
  for(ii in 1:nrow(tmp)){

    if(tmp$icclevel_opp_3[ii]==1){
      prelimTime = prelimTime + 1
    } else if(tmp$icclevel_opp_3[ii]==2){
      formalTime = formalTime + 1
    }
  }

  # organize
  out = data.frame(cntry=x, stringsAsFactors = F)
  out$prelimTime = prelimTime
  out$formalTime = formalTime

  #
  return(out)
})
oppTimes = do.call(rbind, oppTimes)

# get descriptive info
stats = function(x){ c(avg=mean(x), stdev=sd(x)) }
apply(stateTimes[,-1], 2, stats)
apply(oppTimes[,-1], 2, stats)
###############################################################