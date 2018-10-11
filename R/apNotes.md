Ok, we figured out a few different things we'd like to try:

* for ALL of the following models, we'd like to try a dummy version of the p5 alliance variable instead of the current average version

# model 1a: icclevel_opp_3, icclevel_state_3
1. sequential model with a recoded DV with the following categories: 0=0, 1=1, 2=everything else (2-6)

# model 1b: icclevel_opp_4a, icclevel_state_4a
2. sequential model with DV recoded into the following categories: 0=0, 1=1, 2=all 2s and 3s, 3=all 4s, 5s, and 6s

# model 1c: icclevel_opp_4b, icclevel_state_4b
3. sequential model with DV recoded into the following categories: 0=0, 1=1, 2=2, 3=everything else (3=6)

for each of these 3 options, we want to see average coefficients and coefficients broken out by stage, if possible


For the next two options, we want to drop all observations where ICC level=0 from the case universe (so only include ongoing PEs/Formals)    
* again try dummy alliance variable
* try the POI osv variables instead of cumulative OSV variables
* add referral type to the model IF it is already in the data (if not, don't worry about it)

# model 2a: icclevel2_opp_3a, icclevel2_state_3a
4. sequential model with DV recoded into the following categories: 0=all 1s, 1=all 2-3s, 3=all 4-6s

# model 2b: icclevel2_opp_3b, icclevel2_state_3b
5. sequential model with DV recoded into the following categories: 0=all 1s, 1=all 2s, 3=everything else (3-6)

Again we want to see average coefficients and coefficients broken out by stage, if possible


Let me know if any of this is unclear!

===

Ok,
so, we talked through the variables, and I think if possible we want to generate separate coefficients for all of the variables of theoretical interest plus africa.  So, in the existing model, that includes the following:

Africa
osv
affinity scores

The other quick question is about whether there is some way (or what is the best way) to judge the strength, importance, predictive power (or something else along these lines) of the legal variables (judiciary + pts + osv) versus the p5 variables.  this is a secondary question at this point, since we need to settle on final models first, but wanted to raise it as something we'd ideally like to be able to say something about in the paper.

===

thanks, shahryar. we know it's a lot, but we can scale back if needed. hopefully, it helps that we have reduced the # of categories in the DV. 

imho, the following two DVs seems to be most the important, given our constraints. 

A) sequential model with a recoded DV with the following categories: 0=0, 1=1, 2=everything else (2-6)

B) - sequential model with DV recoded into the following categories: 0=all 1s, 1=all 2-3s, 3=all 4-6s
-again, this option drops all observations where ICC level=0 from the case universe (so only include ongoing PEs/Formals)    

both of these options have relatively few categories in the DV.