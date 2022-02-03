library(estimateR)
library(readr)
library(dplyr)
library(ggplot2)

OUT_DIR <- here::here("data", "simulated_incidence")

### Generate Re trajectories from which to simulate outbreaks

## The trajectories are smoothed to be a bit more realistic.
Rt_values_abrupt <- smooth_incidence(c(rep(1.8, times = 133), rep(0.2, times = 17)), data_points_incl = 8)
Rt_values_linear_up <- smooth_incidence(c(rep(1.8, times = 75), seq(from = 0.3, to =  1.2, length.out = 75)), data_points_incl = 8)
Rt_values_linear_down <- smooth_incidence(c(rep(2.3, times = 25), seq(from = 2.3, to =  0.6, length.out = 125)), data_points_incl = 8)
Rt_values_constant <- smooth_incidence(rep(1.25, times = 150), data_points_incl = 8)
Rt_values_abrupt_up <- smooth_incidence(c(rep(1.25, times = 133), rep(3, times = 17)) , data_points_incl = 8)

Rt_values <- data.frame(abrupt = Rt_values_abrupt, 
                        linear_up = Rt_values_linear_up,
                        linear_down = Rt_values_linear_down, 
                        constant = Rt_values_constant,
                        abrupt_up  = Rt_values_abrupt_up)

write_csv(Rt_values, file = file.path(OUT_DIR, "reference_Rt_values.csv"))


## Number of replicates to simulate per parameter configuration
NUM_REPLICATES <- 100

## Seeding of infections
imported_infections <- c(1,1,1,1,1)

# Incubation period - gamma distribution parameters
shape_incubation <- 3.2
scale_incubation <- 2.1
incubation <- list(name="gamma", shape = shape_incubation, scale = scale_incubation)


##### SIMULATION FOR BASIC VALIDATION AND VALIDATION WITH COMBINED OBSERVATIONS
# Delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2.7
scale_onset_to_report <- 2.6
onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

delay <- list(incubation, onset_to_report)

partial_obs_probabilities <- list("case_confirmations" = 0, # The basic validation corresponds to "case_confirmations"
                                  "combined_low_prob" = 0.3, 
                                  "combined_high_prob" = 0.6,
                                  "combined_full_prob" = 1)

Rt_values_names <- colnames(Rt_values)

# Different noise models
noises <- list("noiseless" = list(type = "noiseless"),
               'iid_noise_sd' = list(type = 'iid_noise_sd', sd = 0.1),
               'autocorrelated' = list(type = 'autocorrelated', ar_coeffs = c(0.05, 0.05, -0.02, -0.2), sd = 0.05))

for(noise_name in names(noises)) {
  for(Rt_value_name in Rt_values_names) {
    for(partial_obs_name in names(partial_obs_probabilities)) {

      # Print to keep track of progress
      print(noise_name)
      print(Rt_value_name)
      print(partial_obs_name)

      simulations <- c()
      for(i in 1:NUM_REPLICATES){
        # Simulate infections with estimateR built-in fucntions
        infects <- simulate_infections(Rt_values[[Rt_value_name]], imported_infections)
        simulated_observations <- simulate_combined_observations(infects,
                                                                 incubation,
                                                                 onset_to_report,
                                                                 prob_partial_observation = partial_obs_probabilities[[partial_obs_name]],
                                                                 noise = noises[[noise_name]]) %>%
          mutate(replicate = i)
        simulations <- c(simulations, list(simulated_observations))
      }
      simulations <- bind_rows(simulations)
      # Write result of simulations
      write_csv(simulations, file = file.path(OUT_DIR, paste0(partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")))

    }
  }
}

##### SIMULATION FOR VALIDATION WITH TIME-VARYING DELAYS

# Long delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2
scale_onset_to_report <- 8
long_onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

# Short delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2
scale_onset_to_report <- 2
short_onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

short_to_long_delay_matrix <- estimateR:::.get_delay_distribution(estimateR:::.build_list_of_gradually_changing_delays(init_delay = short_onset_to_report,
                                                                                                                       final_delay  =long_onset_to_report,
                                                                                                                       n_time_steps = nrow(Rt_values)))

long_to_short_delay_matrix <- estimateR:::.get_delay_distribution(estimateR:::.build_list_of_gradually_changing_delays(init_delay = long_onset_to_report,
                                                                                   final_delay= short_onset_to_report,
                                                                                   n_time_steps = nrow(Rt_values)))

delay_changes <- list("short_to_long" = short_to_long_delay_matrix,
                      "long_to_short" = long_to_short_delay_matrix)
partial_obs_name <- "case_confirmations" # only simulate "case_confirmations" case (meaning that p=0)

for(noise_name in names(noises)) {
  for(Rt_value_name in Rt_values_names) {
    for(delay_change_name in names(delay_changes)) {
      print(noise_name)
      print(Rt_value_name)
      print(delay_change_name)

      simulations <- c()
      for(i in 1:NUM_REPLICATES){
        # Simulate infections
        infects <- simulate_infections(Rt_values[[Rt_value_name]], imported_infections)
        simulated_observations <- simulate_combined_observations(infects,
                                                                 incubation,
                                                                 delay_changes[[delay_change_name]],
                                                                 prob_partial_observation = partial_obs_probabilities[[partial_obs_name]],
                                                                 noise = noises[[noise_name]]) %>%
          mutate(replicate = i)
        simulations <- c(simulations, list(simulated_observations))
      }
      simulations <- bind_rows(simulations)
      write_csv(simulations, file = file.path(OUT_DIR, paste0(delay_change_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")))

    }
  }
}
