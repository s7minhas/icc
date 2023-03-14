## Hypothesis 1: 
    - As the gravity of human rights violations perpetrated by government (opposition) actors increases, the likelihood that the OTP initiates a preliminary examination and advances to a formal investigation targeting government (opposition) actors increases
    - Stage 1:
        + govt osv increases, likelihood of prelim exam increases (+)
        + opp osv increases, likelihood of prelim exam increases (+)
    - Stage 2:
        + gov/opp osv increases, likelihood of formal exam decreases (-)

## Hypothesis 2: 
    - The OTP is less likely to initiate preliminary examinations or to advance situations to formal investigation against both governments and opposition actors as the relevant state’s judicial independence increases.
    - Stage 1: 
        + jud indep increases, likelihood of prelim exam decreases FOR BOTH  (+)
    - Stage 2:
        + jud indep mixed support (+/-)

## Hypothesis 3: 
    - As the strength of P5 ties to a given state increases, the likelihood that the OTP initiates a preliminary examination and advances to a formal investigation targeting government or opposition actors in that state decreases.
    - Stage 1: 
        + ideal point no sig effect in gov or opp (+/-)
        + africa no sig eff on prelim exam for gov (+/-)
        + africa sig and pos for opp but who cares (+)
    - Stage 2: 
        + ideal point more likely to advance when states have weaker ties (+) ... so when distance from a p5 on ideal point increases then they are more likely to face a formal investigation
        + afirca (+)

## models run:

x: update the OSV data used to just use GED
    data: GEDEvent_v22_1.RData
    source: https://ucdp.uu.se/downloads/index.html#ged_global
    - plop that in

x: vreeland's polity measure instead of polity
    data: V-Dem-CY-Full+Others-v13.rds
    source: https://www.v-dem.net/data/the-v-dem-dataset/
    - double check timespan
    - and if not available then use 
    vdem

x: three thoughts on predictor variables: 
    data: SVAC_3.0_complete.csv
    source: http://www.sexualviolencedata.org/dataset/
    - migration/ethnic cleansing
    - sexual violence
    - svac data on sexual violence

x: strength of trade ties with p5 instead of voting similarity measure
    - what proportion of their trade is with the p5

x: p2/p3
    - robustness check, 
    - uk, france
    - china, russia
    - us
    - uk, france, us

x: interested in variation in the length of time attributed to such investigations

## to do:

dummy for unsc referral: 
    	# note: for sudan libya turn icclevel into zero, because they were referred from unsc


