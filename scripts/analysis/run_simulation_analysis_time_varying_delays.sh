#!/bin/sh

# TODO: change to the correct dir (folder that contains the ./scripts and ./data)
main_dir="/Users/scirej/Documents/nCov19/Incidence_analysis/estimateR_validation"
cd $main_dir

mkdir -p ./data/simulated_incidence
mkdir -p ./data/inference_on_simulated_incidence
mkdir -p ./plots

# Simulate
echo "Started simulating..."
# Can be commented out if script has already been run at least once before.
Rscript ./scripts/simulation/simulate_incidence_data.R
echo "End of simulations."

Rt_names=("abrupt" "linear_up" "linear_down" "constant" "abrupt_up")
noise_names=("autocorrelated")
delay_variation_names=("short_to_long" "long_to_short")

analysis_script="./scripts/analysis/analyse_simulated_incidence_time_varying_delays.R"

for delay_variation in ${delay_variation_names[*]}; do
  for noise in ${noise_names[*]}; do
    for Rt in ${Rt_names[*]}; do
      echo ${noise}
      echo ${Rt}
      echo ${delay_variation}
      Rscript ${analysis_script} \
      --Rt ${Rt} \
      --noise ${noise} \
      --delay_variation ${delay_variation}
    done
  done
done
