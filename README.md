Code structure: The main code works in 3 main phases: data setup, exploratory analysis, and main analysis. Each phase is summarized below. Scripts run top to bottom, or can use the "here" package to call in/save intermediate datasets (commented out).

Data setup
0_ecog_cog_change_filepaths.R: Sets up filepaths that can be customized to pull in the analytic data
1a_ecog_cog_change_dataset_construction.R: Imports and cleans raw KHANDLE data (Waves 1-4) in preparation for multiple imputation. 
1b_ecog_cog_change_imputation.R: Uses long dataset to perform multiple imputation by chained equations, derives variables for analysis, and creates a ‘mids’ object to be used in linear mixed models.

Exploratory analysis
2_ecog_cog_change_table1.R: Uses long imputed dataset to derive Table 1 statistics and exports data to excel. Extracts the complete case data and creates supplemental Table 1.
2b_ecog_cog_change_sparsity_fig.R: Looking at individual cognitive score slopes, stratified by ECog score

Main analysis
3a_ecog_cog_change_table2.R: Main analysis script that takes the mids object and runs linear mixed models for each outcome and modifier. Also creates fake dataset and uses models to predict on fake data. Exports model information to a Table 2 for each outcome. 
3b_ecog_cog_change_trajectories_FIXED.R: Filters fake datasets so that there are predicted trajectories for each combination of ecog/modifier separately from each model, plots trajectories.
