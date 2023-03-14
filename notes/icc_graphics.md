# scripts to generate data for graphics

    should all be within the analysis/seqOrd_catEffects directory

# inputted data for all graphics: 

    - results/sobState_model1a_1_newp5Var.rda
    - results/sobOpp_model1a_1_newp5Var.rda
    - results/sobState_model1a_1_newp5Var_ptsCivilWarOnly.rda
    - results/sobOpp_model1a_1_newp5Var_ptsCivilWarOnly.rda
    - results/sobState_model1a_1_newp5Var_noImp.rda
    - results/sobOpp_model1a_1_newp5Var_noImp.rda

# main manuscript floats: 

    figure 2: stateCoefSumm.pdf ... state focused icc involvement

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobState_model1a_1_newp5Var.rda

    figure 3: rebelCoefSumm.pdf ... opposition focused icc involvement

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobOpp_model1a_1_newp5Var.rda

# appendix: 

    figure a1: stateCoefTrace.pdf ... trace plot for figure 2

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobState_model1a_1_newp5Var.rda

    figure a2: rebelCoefTrace.pdf ... trace plot for figure 3

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobOpp_model1a_1_newp5Var.rda

    table a1: tabular representation of state model

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobState_model1a_1_newp5Var.rda

    table a2: tabular representation of rebel model

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobOpp_model1a_1_newp5Var.rda


    figure a3: stateCoefSumm_ptsCivilWarOnly.pdf ... state focused when restricting sample to pts > 3 and only icc ratifiers

        generated in: R/analysis/posteriorViz_ptsCivilWarOnly.R
        inputted data: results/sobState_model1a_1_newp5Var_ptsCivilWarOnly.rda

    figure a4: rebelCoefSumm_ptsCivilWarOnly.pdf ... opposition focused when restricting sample to pts > 3 and only icc ratifiers

        generated in: R/analysis/posteriorViz_ptsCivilWarOnly.R
        inputted data: results/sobOpp_model1a_1_newp5Var_ptsCivilWarOnly.rda

    figure a5: stateCoefSumm_noImp.pdf ... state focused with no imputation

        generated in: R/analysis/posteriorViz_noImp.R
        inputted data: results/sobState_model1a_1_newp5Var_noImp.rda

    figure a6: rebelCoefSumm_noImp.pdf ... opposition focused with no imputation

        generated in: R/analysis/posteriorViz_noImp.R
        inputted data: results/sobOpp_model1a_1_newp5Var_noImp.rda

    figure a7: modCompare_state.pdf ... state focused model comparisons with alternative estimation frameworks

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobState_model1a_1_newp5Var.rda        

    figure a8: modCompare_opp.pdf ... opposition focused model comparisons with alternative estimation frameworks

        generated in: R/analysis/posteriorViz.R
        inputted data: results/sobOpp_model1a_1_newp5Var.rda

    figure a9: somerViz.pdf ... performance comparison across alternative estimation approaches using somer's d

        generated in: R/analysis/posteriorViz.R
        inputted data:
            results/sobState_model1a_1_newp5Var.rda 
            results/sobOpp_model1a_1_newp5Var.rda    