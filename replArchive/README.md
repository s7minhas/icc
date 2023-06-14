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
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] countrycode_1.4.0 devtools_2.4.5    usethis_2.1.6     xtable_1.8-4     
 [5] doParallel_1.0.17 iterators_1.0.14  foreach_1.5.2     Cairo_1.6-0      
 [9] latex2exp_0.9.6   ggplot2_3.4.2     tidyr_1.3.0       lubridate_1.9.2  
[13] magrittr_2.0.3    reshape2_1.4.4    dplyr_1.1.2       haven_2.5.2      
[17] foreign_0.8-82   

loaded via a namespace (and not attached):
 [1] utf8_1.2.2        generics_0.1.3    stringi_1.7.12    digest_0.6.29    
 [5] hms_1.1.3         grid_4.3.0        timechange_0.2.0  pkgload_1.3.2    
 [9] fastmap_1.1.1     plyr_1.8.8        jsonlite_1.8.4    processx_3.8.1   
[13] sessioninfo_1.2.2 pkgbuild_1.4.0    urlchecker_1.0.1  ps_1.6.0         
[17] promises_1.2.0.1  purrr_1.0.1       fansi_1.0.2       scales_1.2.1     
[21] codetools_0.2-19  cli_3.6.1         shiny_1.5.0       crayon_1.5.0     
[25] rlang_1.1.1       ellipsis_0.3.2    munsell_0.5.0     remotes_2.4.2    
[29] withr_2.5.0       cachem_1.0.8      tools_4.3.0       memoise_2.0.1    
[33] colorspace_2.0-2  httpuv_1.6.5      forcats_1.0.0     mime_0.12        
[37] vctrs_0.6.2       R6_2.5.1          lifecycle_1.0.3   stringr_1.5.0    
[41] htmlwidgets_1.6.2 fs_1.5.2          miniUI_0.1.1.1    callr_3.7.3      
[45] pkgconfig_2.0.3   later_1.3.0       pillar_1.9.0      gtable_0.3.0     
[49] profvis_0.3.8     glue_1.6.2        Rcpp_1.0.10       tibble_3.2.1     
[53] tidyselect_1.2.0  htmltools_0.5.5   compiler_4.3.0    prettyunits_1.1.1

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
- Figures 2 and 3 are constructed manually and are not based on any code.
- Table 1 is constructed manually. The parameter listed in that table accord with the simulation parameters used in `compModelSim/abmRunning.py`.
- **figure_4.R**: Summarizes the results of a regression analysis on the simulated data. The `figure_4.R` script just create a coefficient plot using two inputs: `results/abm_feCoefs.rda` and `results/abm_reCoefs.rda`. The output of this script is stored in `graphics/figure4.png`. Steps to reproduce these results from scratch involve running the computational model, processing the data, and conducting the regression analysis; each of the scripts necessary to perform these steps are in the `compModelSim` directory:
  - `1_abmRunning.py`: Runs the computational model using the parameters specified in the paper, the code for the model itself is located in `0_VicForViz.py` (This file takes several hours to run).
  - `2_getVicCount.R`: Calculates victimization counts from the game.
  - `3_getNetStats.R`: Calculates network statistics from the game (This file utilizes parallelization, adjust number of cores in the script).
  - `4_abmDataPrep.R`: Organizing the data for analysis.
  - `5_fe_abmAnalysis.R`: Runs the fixed effects regression analysis (This file takes several hours to run).
  - `5_re_abmAnalysis.R`: Runs the random effects regression analysis.
- **figure_5.R**: Creates a descriptive visualization of how the number of actors in armed conflicts have changed over time as well as our key independent variable, network competition. The inputs for this script are: `data/rawModelData.rda` and `data/actorCntsID.rda`. The output of this script is stored in `graphics/figure5.png`.
- Table 2 is constructed manually.
- **figure_6_7.R**: Summarizes the results of a regression analysis on data from ACLED. The script requires three inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. The output of this script is stored in `graphics/figure6.png` and `graphics/figure7.png`. Steps to reproduce these results from scratch involve imputing data, running base model, running model with first set of controls, and running model with second set of controls; each of the scripts necessary to perform these steps are in the `acledAnalysis` directory:
  - `1_genImpData.R`: Performs multiple imputation analysis.
  - `2a_runModels_base.R`: Runs models without any controls not being measured from ACLED.
  - `2b_runModels_cnt1.R`: Runs models with first set of controls specified in the manuscript.
  - `2c_runMOdels_cnt2.R`: Runs models with second set of controls specified in the manuscript.
- **figure8.R**: Performs a simulation analysis to estimate the substantive effect of network competition across the model results presented in the manuscript. The script requires three inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. The output of this script is stored in `graphics/figure8.png`. The inputs for this script should have already been generated when following the steps laid out for `figure_6_7.R`.

#### Reproducing figures in the appendix

All of the scripts necessary to reproduce the figures in the appendix are located in the `appendix/` directory.

- **01_tableA_1.R**: Generates Table A1 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/table_A1.tex`.
- **02_tableA_2_3_4.R**: Generates Tables A2-4 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/table_A2.tex`, `graphics/appendix/table_A3.tex`, and `graphics/appendix/table_A4.tex`.
- **03_figureA_1.R**: Generates Figure A1 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A1.png`.
- **04_figureA_2_3_4.R**: Generates Figures A2-4 in the Appendix. Inputs: `results/baseMods.rda`, `results/cnt1Mods.rda`, and `results/cnt2Mods.rda`. Outputs: `graphics/appendix/figure_A2.png`, `graphics/appendix/figure_A3.png`, and `graphics/appendix/figure_A4.png`.
- **05_figureA_5_6.R**: Generates Figures A5-6 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A5.png` and `graphics/appendix/figure_A6.png`.
- **06_figureA_7_8.R**: Generates Figures A7-8 in the Appendix. Inputs: `data/rawModelData.rda` and `data/geoSpread_acled.rda`. Outputs: `graphics/appendix/figure_A7.png` and `graphics/appendix/figure_A8.png`.
- **07_figureA_9.R**: Generates Figure A9 in the Appendix. Inputs: `results/abm_feCoefs_allyProp.rda` and `results/abm_reCoefs_allyProp.rda`. Outputs: `graphics/appendix/figure_A9.png`.
- **08_figureA_10_11.R**: Generates Figures A10-11 in the Appendix. Inputs: `data/rawModelData.rda` and `data/allyProp_acled.rda`. Outputs: `graphics/appendix/figure_A10.png` and `graphics/appendix/figure_A11.png`.
- **09_figureA_12.R**: Generates Figure A12 in the Appendix. Inputs: `data/netStats.rda`. Outputs: `graphics/appendix/figure_A12.png`.
- **10_figureA_13.R**: Generates Figure A13 in the Appendix. Inputs: `data/modelDataCnt2.rda`. Outputs: `graphics/appendix/figure_A13.png`.
- **11_figureA_14.R**: Generates Figure A14 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A14.png`.
- **12_figureA_15.R**: Generates Figure A15 in the Appendix. Inputs: `data/data.rda`. Outputs: `graphics/appendix/figure_A15.png`.
- **13_figureA_16.R**: Generates Figure A16 in the Appendix. Inputs: `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A16.png`.
- **14_figureA_17_18.R**: Generates Figure A17-18 in the Appendix. Inputs: `data/GEDEvent_v21_1.RData`, `data/rawModelData.rda`, and `data/modelDataCnt2.rda`. Outputs: `graphics/appendix/figure_A17.png` and `graphics/appendix/figure_A18.png`.
- **15_figureA_19_20.R**: Generates Figure A19-20 in the Appendix. Inputs: `data/acledCiv.rda` and `data/rawModelData.rda`. Outputs: `graphics/appendix/figure_A19.png` and `graphics/appendix/figure_A20.png`.


#### R package build notes

Below we provide the version of each of the libraries that our project relies on (each library was built using R 4.0.5). We use one package, `simHelper`, that is not available on CRAN and needs to be installed using devtools, this can be done via executing the following R code: `devtools::install_github('s7minhas/simHelper', ref='vic')`. Version information for the other libraries used in the analysis are shown below:

|                |                   |                 |                  |
|:---------------|:------------------|:----------------|:-----------------|
|abind 1.4-5     |Cairo 1.5-12.2     |countrycode 0.16 |doParallel 1.0.16 |
|dplyr 1.0.6     |extrafont 0.17     |foreach 1.5.1    |ggplot2 3.3.5     |
|ggraph 2.0.5    |glmmADMB 0.8.3.3   |glmmTMB 1.0.2.1  |igraph 1.2.6      |
|latex2exp 0.5.0 |magrittr 2.0.1     |MASS 7.3-54      |network 1.17.1    |
|patchwork 1.1.1 |RColorBrewer 1.1-3 |readr 1.4.0      |reshape2 1.4.4    |
|sbgcop 0.980    |sna 2.6            |stringr 1.4.0    |tidygraph 1.2.0   |
|tidyr 1.1.3     |xtable 1.8-4       |                 |                  |


If you find any errors or have any further questions, please address them to me via email at minhassh@msu.edu.
