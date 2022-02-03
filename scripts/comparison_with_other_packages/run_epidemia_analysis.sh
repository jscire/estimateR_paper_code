#!/bin/sh

# TODO: change to the correct dir (folder that contains the ./scripts and ./data)
main_dir="/Users/scirej/Documents/nCov19/Incidence_analysis/estimateR_validation"
cd $main_dir

mkdir -p ./data/simulated_incidence
mkdir -p ./data/epidemia

# Simulate
# Comment out simulation bit if simulation script has already been run
echo "Started simulating..."
Rscript ./scripts/simulation/simulate_incidence_data.R
echo "End of simulations."

Rt_names=("abrupt" "linear_up" "linear_down" "constant" "abrupt_up")
noise_names=("iid_noise_sd")
partial_obs_prob_names=("case_confirmations")

analysis_script="./scripts/comparison/computation_epidemia.R"

for partial_obs_prob in ${partial_obs_prob_names[*]}; do
  for noise in ${noise_names[*]}; do
    for Rt in ${Rt_names[*]}; do
      echo ${noise}
      echo ${Rt}
      echo ${partial_obs_prob}
      Rscript ${analysis_script} \
      --Rt ${Rt} \
      --noise ${noise} \
      --partial_obs_prob ${partial_obs_prob}
    done
  done
done
