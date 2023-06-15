## Replication instructions for the paper

The base directory of the replication archive contains all the figures and tables shown in the paper. There are three subdirectories that contain the code necessary to produce these outputs (note that these files are also available on Github at [https://github/s7minhas/icc](https://github.com/s7minhas/icc)):

- **descriptive**: contains the code necessary to reproduce descriptive statistics reported in the paper
- **analysis**: contains the code necessary for running the empirical analysis in the paper
- **appendix**: contains the code necessary to reproduce all the figures in the Appendix
- **functions**: contains a set of R scripts with helper functions used in the analysis
- **data**: contains the data files used in the analysis
- **results**: contains the results of model runs
- **graphics**: contains .pngs and .tex files for all the figures and tables in the manuscript and appendix

Replicating the figures and tables in the **main** text will take only a few minutes on a standard laptop if the provided `.rda` files are used.

#### Setup information

All of the analyses reported in the manuscript and the appendix are run with the following specification (further information on packages used in the analysis is included at the end of the README):

##### R information

```
> sessionInfo()
R version 4.3.0 (2023-04-21)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Pop!_OS 22.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: America/Detroit
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.3.0 cli_3.6.1      jsonlite_1.8.5 rlang_1.1.1 

> benchmarkme::get_cpu()
$vendor_id
[1] "AuthenticAMD"

$model_name
[1] "AMD Ryzen 9 7950X 16-Core Processor"

$no_of_cores
[1] 32

> benchmarkme::get_ram()
134 GB
```

#### Reproducing figures in the manuscript

The `here` package is used to manage paths in this replication archive for R scripts. The .here file is stored in the top-most folder of the replication archive. Scripts are listed in order of the figures and tables in the manuscript. 

- **descriptive/fig_1_iccMaps.R**: Creates visualization showcasing spatial distribution of ICC cases. Takes `data/mergedData_yrly_ongoing.rda` and `data/panel.rda` as inputs. Output is stored in `graphics/iccMaps.png`.
- **analysis/figs_2_3_a1_a2_main.R**: Summarizes the results of the main regression analysis presented in the paper. The inputs for this script are: `results/sobOpp_model1a_1_newp5Var_fin.rda` and `results/sobState_model1a_1_newp5Var_fin.rda`. The output of this script is stored in `graphics/fig_2.png` and `graphics/fig_3.png`. Additional files for the appendix are generated from this script as well. Steps to reproduce these results from scratch involve running two scripts:
  - `analysis/sob_opp.R`: Runs the Opposition-Focused ICC Involvement model. The inputs for this script are `data/modData_fin.rda` and `data/sobOpp_imp_fin.rda`. The output of this model is a `brms` object saved as `results/sobOpp_model1a_1_newp5Var_fin.rda`.
  - `analysis/sob_state.R`: Runs the State-Focused ICC Involvement model. The inputs for this script are `data/modData_fin.rda` and `data/sobState_imp_fin.rda`. The output of this model is a `brms` object saved as `results/sobState_model1a_1_newp5Var_fin.rda`.

#### Reproducing figures in the appendix

All of the scripts necessary to reproduce the figures in the appendix are located in the `appendix/` directory.

- **appendix/figs_2_3_a1_a2_main.R**: This script also generates the visualizations shown in Sections A.2 and A.3 of the Appendix. The inputs are the same as mentioned above and the outputs are: `graphics/fig_a1.png`, `graphics/fig_a2.png`, `graphics/tab_a1.tex`, and `graphics/tab_a2.tex`. 
- **appendix/fig_a_ptsCivilWarOnly.R**: Produces the results shown in Section A.4. The inputs for this script are: `results/sobOpp_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda` and `results/sobState_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda`. The output of this script is stored in `graphics/fig_a_ptsCivilWarOnly.png`. Steps to reproduce these results from scratch: 
  - `appendix/ptsCivilWarOnly/sob_opp.R`: Runs the Opposition model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/subset_ptsCivWar_cntries.rda`. The output is: `results/sobOpp_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda`. 
  - `appendix/ptsCivilWarOnly/sob_state.R`: Runs the State model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/subset_ptsCivWar_cntries.rda`. The output is: `results/sobState_model1a_1_newp5Var_ptsCivilWarOnly_fin.rda`.   
- **appendix/fig_a_noImp.R**: Produces the results shown in Section A.5. The inputs for this script are: `results/sobOpp_model1a_1_newp5Var_noImp_fin.rda` and `results/sobState_model1a_1_newp5Var_noImp_fin.rda`. The output of this script is stored in `graphics/fig_a_noImp.png`. Steps to reproduce these results from scratch: 
  - `appendix/noImp/sob_opp.R`: Runs the Opposition model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobOpp_model1a_1_newp5Var_noImp_fin.rda`.
  - `appendix/noImp/sob_state.R`: Runs the State model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobState_model1a_1_newp5Var_noImp_fin.rda`.
- **appendix/fig_a_democ.R**: Produces the results shown in Section A.6. The inputs for this script are: `results/sobOpp_democ_fin.rda` and `results/sobState_democ_fin.rda`. The output of this script is stored in `graphics/fig_a_democ.png`. Steps to reproduce these results from scratch: 
  - `appendix/democ/sob_opp.R`: Runs the Opposition model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/V-Dem-CY-Full+Others-v13.rds`. The output is: `results/sobOpp_democ_fin.rda`. 
  - `appendix/democ/sob_state.R`: Runs the State model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/V-Dem-CY-Full+Others-v13.rds`. The output is: `results/sobState_democ_fin.rda`.
- **appendix/fig_a_svac.R**: Produces the results shown in Section A.7. The inputs for this script are: `results/sobOpp_svac_fin.rda` and `results/sobState_svac_fin.rda`. The output of this script is stored in `graphics/fig_a_svac.png`. Steps to reproduce these results from scratch: 
  - `appendix/svac/sob_opp.R`: Runs the Opposition model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/SVAC_3.0_complete.csv`. The output is: `results/sobOpp_svac_fin.rda`. 
  - `appendix/svac/sob_state.R`: Runs the State model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/SVAC_3.0_complete.csv`. The output is: `results/sobState_svac_fin.rda`.
- **appendix/fig_a_p_statePrefAlt.R**: Produces the results shown in Section A.8. The inputs for this script are: `results/sobOpp_statePrefAlt_fin.rda` and `results/sobState_statePrefAlt_fin.rda`. The output of this script is stored in `graphics/fig_a_p_statePrefAlt.png`. Steps to reproduce these results from scratch: 
  - `appendix/statePrefAlt/sob_opp.R`: Runs the Opposition model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/statePref_from_plutonium.rda`. The output is: `results/sobOpp_statePrefAlt_fin.rda`. 
  - `appendix/statePrefAlt/sob_state.R`: Runs the State model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/statePref_from_plutonium.rda`. The output is: `results/sobState_statePrefAlt_fin.rda`.  
- **appendix/fig_a_p_US_RUS_CHN.R**: Produces the results shown in Section A.9. The inputs for this script are: `results/sobOpp_p_US_RUS_CHN_fin.rda` and `results/sobState_p_US_RUS_CHN_fin.rda`. The output of this script is stored in `graphics/fig_a_p_US_RUS_CHN.png`. Steps to reproduce these results from scratch: 
  - `appendix/p_US_RUS_CHN/sob_opp.R`: Runs the Opposition model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobOpp_p_US_RUS_CHN_fin.rda`. 
  - `appendix/p_US_RUS_CHN/sob_state.R`: Runs the State model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobState_p_US_RUS_CHN_fin.rda`.
- **appendix/fig_a_unsc.R**: Produces the results shown in Section A.10. The inputs for this script are: `results/sobOpp_unsc_fin.rda` and `results/sobState_unsc_fin.rda`. The output of this script is stored in `graphics/fig_a_unsc.png`. Steps to reproduce these results from scratch: 
  - `appendix/unsc/sob_opp.R`: Runs the Opposition model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobOpp_unsc_fin.rda`. 
  - `appendix/unsc/sob_state.R`: Runs the State model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobState_unsc_fin.rda`.
- **appendix/fig_a_chiefProsec.R**: Produces the results shown in Section A.11. The inputs for this script are: `results/sobOpp_chiefProsec_fin.rda` and `results/sobState_chiefProsec_fin.rda`. The output of this script is stored in `graphics/fig_a_chiefProsec.png`. Steps to reproduce these results from scratch: 
  - `appendix/chiefProsec/sob_opp.R`: Runs the Opposition model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobOpp_chiefProsec_fin.rda`. 
  - `appendix/chiefProsec/sob_state.R`: Runs the State model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobState_chiefProsec_fin.rda`.
- **appendix/fig_a_iccTime.R**: Produces the results shown in Section A.12. The inputs for this script are: `results/sobOpp_iccTime_fin.rda` and `results/sobState_iccTime_fin.rda`. The output of this script is stored in `graphics/fig_a_iccTime.png`. Steps to reproduce these results from scratch: 
  - `appendix/iccTime/sob_opp.R`: Runs the Opposition model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobOpp_iccTime_fin.rda`. 
  - `appendix/iccTime/sob_state.R`: Runs the State model for this check. The input for this script is `data/modData_fin.rda`. The output is: `results/sobState_iccTime_fin.rda`.
- **appendix/fig_a_impleLegDom.R**: Produces the results shown in Section A.13. The inputs for this script are: `results/sobOpp_impleLegDom_fin.rda` and `results/sobState_impleLegDom_fin.rda`. The output of this script is stored in `graphics/fig_a_impleLegDom.png`. Steps to reproduce these results from scratch: 
  - `appendix/impleLegDom/sob_opp.R`: Runs the Opposition model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/imple_leg_domestic.csv`. The output is: `results/sobOpp_impleLegDom_fin.rda`. 
  - `appendix/impleLegDom/sob_state.R`: Runs the State model for this check. The inputs for this script are: `data/modData_fin.rda` and `data/imple_leg_domestic.csv`. The output is: `results/sobState_impleLegDom_fin.rda`.
- **appendix/figs_a_alt_models_coef_state_opp.R**: Produces Figures A13 and A14 shown in Section A.14. The inputs for this script are: `results/sobOpp_model1a_1_newp5Var_fin.rda` and `results/sobState_model1a_1_newp5Var_fin.rda`. The output of this script is stored in: `graphics/fig_a_alt_models_coef_state.png` and `graphics/fig_a_alt_models_coef_opp.png`.
- **appendix/figs_a_model_perf.R**: Produces Figure A15 shown in Section A.14. The inputs for this script are: `results/sobOpp_model1a_1_newp5Var_fin.rda`, `results/sobState_model1a_1_newp5Var_fin.rda`, and `results/perfResults_fin.rda`. The output of this script is stored in `graphics/fig_a_model_perf.png`.
- **descriptive/fig_a16_descStats.R**: Produces the results shown in Section A.15. The inputs for this script are: `data/mergedData_yrly_ongoing.rda`. The output of this script is stored in `graphics/fig_a13.png`.

#### R package build notes

Below we provide the version of each of the libraries that our project relies on (each library was built using R 4.3.0). Version information for the other libraries used in the analysis are shown below:

|                 |                  |                 |                    |                    |
|:----------------|:-----------------|:----------------|:-------------------|:-------------------|
|bayesplot 1.10.0 |doParallel 1.0.17 |ggplot2 3.4.2    |magrittr 2.0.3      |sbgcop 0.980        |
|brms 2.19.0      |dplyr 1.1.2       |gridExtra 2.3    |MASS 7.3-59         |sf 1.0-13           |
|Cairo 1.6-0      |extrafont 0.19    |haven 2.5.2      |RColorBrewer 1.1-3  |tadaatoolbox 0.17.0 |
|countrycode 0.16 |foreach 1.5.2     |hrbrthemes 0.8.0 |reshape2 1.4.4      |tidyr 1.3.0         |
|cowplot 1.1.1    |foreign 0.8-82    |latex2exp 0.9.6  |rgeos 0.6-3         |VGAM 1.1-8          |
|devtools 2.4.5   |future 1.32.0     |lubridate 1.9.2  |rnaturalearth 0.3.3 |xtable 1.8-4        |

If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.
