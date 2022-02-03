library(estimateR)
library(readr)
library(tidyr)
library(dplyr)
library(Metrics)
library(ggplot2)

INFERENCE_DIR <-  here::here("data", "inference_on_simulated_incidence")
SIMULATION_DIR <-  here::here("data", "simulated_incidence")

Rt_values_names <- c("abrupt", "abrupt_up", "linear_up", "linear_down", "constant")

noise_names <- c("noiseless", 'iid_noise_sd', 'autocorrelated' ) 

partial_obs_name <- "case_confirmations"

delay_changes <- list("short_to_long",
                      "long_to_short")

analysis_delays <- list("short",
                        "long",
                        "short_to_long",
                        "long_to_short")

bootstrapping_names <- c("bootstrapped", "SI_bootstrapped")

# Threshold on the fraction of analyses with a valid (non-NA) Re value for a particular time step.
# Below this threshold the aggregated Re value is not computed for the time step of interest.
success_rate_threshold <- 0.95

# Read ground truth values of Re
reference_Rt_values <-read_csv(file = file.path(SIMULATION_DIR, "reference_Rt_values.csv"),
                               col_types = list(
                                 abrupt = col_double(),
                                 linear_up = col_double(),
                                 linear_down = col_double(),
                                 constant = col_double())) %>% 
  mutate(idx= 1:150) %>% 
  pivot_longer(cols = -idx,
               names_to = "Rt_type",
               values_to = "ref_Rt") %>% 
  arrange(Rt_type, idx)

# Read all relevant analyses results and merge them into a single dataframe
all_results <- list()
for(bootstrapping in bootstrapping_names) {
  for(noise_name in noise_names) {
    for(Rt_value_name in Rt_values_names) {
      for(delay_change in delay_changes) {
        for(analysis_delay in analysis_delays) {
          simulation_results <- try(read_csv(file =  file.path(INFERENCE_DIR, 
                                                               paste0(bootstrapping,"_inference_on_", 
                                                                      delay_change, "_delay_",
                                                                      analysis_delay, "_Rt_",
                                                                      Rt_value_name, "_", 
                                                                      noise_name, ".csv"))),
                                    silent = TRUE)
          if ("try-error" %in% class(simulation_results)) {
            # We catch errors to handle cases when simulations have not produced a successful outbreak.
            cat("File not found:\t")
            cat(paste0(bootstrapping,"_inference_on_", 
                       delay_change, "_delay_",
                       analysis_delay, "_Rt_",
                       Rt_value_name, "_", 
                       noise_name, ".csv\n"))
          } else {
            simulation_results <- simulation_results %>% 
              mutate(Rt_type = Rt_value_name,
                     simulation_type = "case_confirmations",
                     delay_change_type = delay_change,
                     analysis_delay = analysis_delay,
                     noise_type = noise_name,
                     bootstrapping_type = bootstrapping)
            
            all_results <- c(all_results, list(simulation_results))
          }
        }
      }
    }
  }
}


all_results <- bind_rows(all_results)

# Add ground truth values to summary dataframe
results_with_ref <- full_join(reference_Rt_values, all_results, by = c("idx", "Rt_type"))

# Compute coverage
coverage_computation <- results_with_ref %>% 
  mutate(ref_value_in_CI = case_when(ref_Rt >= CI_down_Re_estimate & ref_Rt <= CI_up_Re_estimate ~ 1,
                                     is.na(ref_Rt) | is.na(CI_down_Re_estimate) | is.na(CI_up_Re_estimate) ~ NA_real_,
                                     TRUE ~ 0),
         successful_computation = case_when(is.na(Re_estimate) ~ 0,
                                            TRUE ~ 1))

# Compute RMSE and summarized Re values
summary_computation <- coverage_computation %>% 
  group_by(idx, Rt_type, simulation_type, noise_type, bootstrapping_type, delay_change_type, analysis_delay) %>% 
  summarise(rmse = rmse(case_when(successful_computation == 1 ~ ref_Rt,
                                  TRUE ~ 0), 
                        case_when(successful_computation == 1 ~ Re_estimate, 
                                  TRUE ~ 0)),
            coverage = mean(ref_value_in_CI, na.rm = TRUE),
            median_Re_estimate = median(Re_estimate, na.rm = TRUE),
            median_CI_down_Re_estimate = median(CI_down_Re_estimate, na.rm = TRUE),
            median_CI_up_Re_estimate = median(CI_up_Re_estimate, na.rm = TRUE),
            success_rate = mean(successful_computation, na.rm = TRUE),
            ref_Rt = ref_Rt,
            .groups = "drop") %>% 
  mutate(rmse = case_when(success_rate < success_rate_threshold ~ NA_real_,
                          TRUE ~ rmse),
         coverage = case_when(success_rate < success_rate_threshold ~ NA_real_,
                              TRUE ~ coverage),
         median_Re_estimate = case_when(success_rate < success_rate_threshold ~ NA_real_,
                                        TRUE ~ median_Re_estimate),
         median_CI_down_Re_estimate = case_when(success_rate < success_rate_threshold ~ NA_real_,
                                                TRUE ~ median_CI_down_Re_estimate),
         median_CI_up_Re_estimate = case_when(success_rate < success_rate_threshold ~ NA_real_,
                                              TRUE ~ median_CI_up_Re_estimate)) %>% 
  arrange(bootstrapping_type, Rt_type, simulation_type, noise_type, delay_change_type, analysis_delay, idx)

# Recode factors for easier plotting
summary_computation$simulation_type <-  recode_factor(summary_computation$simulation_type, 
                                                      case_confirmations = "Case confirmations",
                                                      combined_low_prob = "Combined - Low prob.",
                                                      combined_high_prob = "Combined - High prob.",
                                                      combined_full_prob = "Symptom onsets")

summary_computation$analysis_delay <-  recode_factor(summary_computation$analysis_delay, 
                                                        short = "Short",
                                                      long = "Long",
                                                      short_to_long = "Short to long",
                                                      long_to_short = "Long to short")

summary_computation$Rt_type <-  recode_factor(summary_computation$Rt_type, 
                                              abrupt = "Abrupt decrease",
                                              abrupt_up = "Abrupt increase",
                                              constant = "Constant",
                                              linear_down = "Linear decrease",
                                              linear_up = "Linear increase")

# Save result
write_csv(summary_computation, file = file.path(INFERENCE_DIR, "summary_inference_on_simulations_time_varying_delays.csv"))
