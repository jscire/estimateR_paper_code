# README

The code in this repository allows the replication of the validation and analysis presented in Scire et al. 2022.
For ease of use, download the entire repository and create an RScript project in the root folder.

## Instructions for estimateR validation

1. In RStudio, create a project (from existing files) in the same folder as this README.md file
2. Use bash files in scripts/analysis to simulate and analyse simulated data
3. Run corresponding R scripts in scripts/result_aggregation
4. Plot corresponding figures with R scripts in scripts/plotting

Note for step 2.
Running 'run_simulation_analysis.sh' or 'run_simulation_analysis_time_varying_delays.sh'
can take a while. To speed things up,
1. Comment out the simulation part once it has been run once.
2. Some parameter combinations can be ignored by altering removing values in
    the `Rt_names`, `noise_names` and `partial_obs_prob_names` arrays.
3. Otherwise, the number of replicates in 'scripts/simulation/simulate_incidence_data.R'
can be reduced.

## Comparison on simulated data

### Comparison of estimateR with Huisman et al. method
1. Set up a local clone of the estimateR [github repository](https://github.com/covid-19-Re/estimateR)
2. Checkout the 'old_right_correction' branch: run `git checkout old_right_correction` in the repository folder
3. Open the estimateR code project in RStudio
4. Re-build the estimateR package in RStudio to update the package version
   (Cmd-Shift-B on MAcOs, or click 'Install and Restart' in 'Build' panel)
5. Restart R (Session/Restart R)
6. Run scripts/comparison_simulated/compare_old_vs_new_correction.R
7. Run scripts/comparison_simulated/compare_old_vs_new_leftpadding.R
8. Run scripts/comparison_simulated/compare_old_vs_new_rightpadding.R
9. When done, you can undo the change in estimateR version by re-installing estimateR:
    either by doing the same procedure on the master branch of the estimateR repository
    or (better) by running: `install_github("covid-19-Re/estimateR")` in RStudio.
    Restart R to apply changes.

### Comparison of estimateR with epidemia and EpiNow2
1. Run basic estimateR validation if not done already
    - Run scripts/analysis/run_simulation_analysis.sh
    - Run scripts/result_aggregation/compare_inference_to_original.R
    - Run scripts/result_aggregation/summarize_run_time.R
2. Run epidemia and EpiNow2 analyses of the simulated data
    - Run scripts/comparison_simulated/run_epidemia(epinow2)_analysis.sh
    - Run scripts/comparison_simulated/compare_epidemia(EpiNow2)_inference_to_original.R
3. Plot figures: run scripts/plotting/plot_figure_package_comparison.R

Note: to speed things up, reduce the number of replicates in computation_epidemia(EpiNow2).R

## Application to empirical data

### COVID-19
1. Run scripts/comparison_COVID-19/analyze_empirical_data.R
2. Run scripts/comparison_COVID-19/epiforcasts_EpiNow.R
3. Run scripts/plotting/plot_figure_empirical_comparison.R
Notes:
  - Huisman et al. pipeline estimates can be downloaded from https://github.com/covid-19-Re/dailyRe-Data,
  relevant estimates are already placed in data/empirical_data
  - Epiforcasts estimates can be downloaded from https://github.com/epiforecasts/covid-rt-estimates/blob/master/national/cases/summary/rt.csv. Note that the present script downloads the full history of Rt estimates from the repository.
  - The Swiss line list informing the analysis summarized in
data/empirical_data_results/2021-10-19_swiss-estimates.csv is not publicly available
and thus we directly provide the analysis result.

### Dengue fever
1. Run scripts/comparison_dengue/dengue_preprocess.R
2. Run scripts/comparison_dengue/dengue_estimation.Rmd