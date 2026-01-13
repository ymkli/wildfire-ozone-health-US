# Growing Impacts of Fire Smoke on Ozone Pollution and Associated Mortality Burden in the United States
Replication materials for Li et al. (2026) 'Growing Impacts of Fire Smoke on Ozone Pollution and Associated Mortality Burden in the United States'

The materials in this repository allow users to reproduce the main results and figures of the paper.

If you have questions or suggestions, please contact Yangmingkai Li at yangmingkai.li@stonybrook.edu.

## Organization of repository
- Scripts/figures: scripts for producing the figures.
- Data/input: data inputs for analysis.

## Instructions
The analysis is performed using R (version 4.2.2). Packages needed are listed at the beginning of each script. 

### Scripts/figures:

scripts for producing Figure 1-6 in the main paper. 


### Data/input

- *runoff_generation_month_final.rds*

The primary dataset to perform the regressions to estimate runoff impacts on fossil fuel generation.


- *runoff_month_1980_2021.rds*

The runoff anomalies from 1980 - 2021. Used to produce Figure 1.

- *region_monthly_covariates.rds* 

The regional and monthly level covariates used for regression analysis and various analyses. Covariates include demand and aggregated electricity generation from various sources.

- *fuel_gas_consumption_2001_2021.rds*

The plant-level natural gas consumption data from EIA. Used to calculate the plant-level CH4 usage and leakage.

- *monitor_drought_emis.rds* 

The dataset to perform regressions to estimate the impacts of drought-induced emissions on PM2.5 measured at monitors. The dataset contains drought-induced emissions from different distance bins for each EPA monitor, and the non-smoke PM2.5 measured at each monitor.

- *monthly_emis_cmip6_scenarios.rds*

The input data to estimate future damages of drought-induced fossil fuel generation. The data includes the drought-induced generation and emissions from each plant under different climate model projections and electricity sector scenarios.  





-*emis_highRE_2024_2050_nerc_bystate.rds*

The predicted drought-induced emissions under high RE scenarios at two conditions (average years and dry year). Used to plot Figure 5D.
