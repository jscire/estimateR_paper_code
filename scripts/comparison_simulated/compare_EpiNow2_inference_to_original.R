library(readr)
library(tidyr)
library(dplyr)
library(Metrics)
library(ggplot2)
library(lubridate)

INFERENCE_DIR <-  here::here("data", "epinow2")
SIMULATION_DIR <-  here::here("data", "simulated_incidence")

Rt_values_names <- c("abrupt", "linear_up", "linear_down", "constant", "abrupt_up")
simulation_type <- "case_confirmations"
noise_type <- "iid_noise_sd"
method_name <- "epinow2"

success_rate_threshold <- 0.9
ref_date <- as.Date("2020-01-01")

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

all_results <- list()
num_replicates <- 100

for(Rt_value_name in Rt_values_names) {
  for(replicate_id in 1:num_replicates) {
    simulation_results <- try(read_csv(file =  file.path(INFERENCE_DIR,
                                                         paste0(method_name,"_inference_on_",
                                                                simulation_type, "_Rt_",
                                                                Rt_value_name, "_",
                                                                noise_type,
                                                                "_replicate_", replicate_id,
                                                                ".csv"))),
                              silent = TRUE)
    if ("try-error" %in% class(simulation_results)) {
      # We catch errors to handle cases when simulations have not produced a successful outbreak.
      cat("File not found")
      cat(paste0(method_name,"_inference_on_",
                 simulation_type, "_Rt_",
                 Rt_value_name, "_",
                 noise_type,"_replicate_",
                 replicate_id, ".csv"))
    } else {
      
      simulation_results <- simulation_results %>%
        filter(type == "estimate") %>% # We filter out the nowcast estimates
        group_by(replicate) %>% 
        mutate(idx = as.integer(min(date) - ref_date) + row_number()) %>% 
        ungroup() %>% 
        select(idx, mean, upper_95, lower_95, run_time, replicate) %>% 
        rename(Re_estimate= mean,
               CI_up_Re_estimate = upper_95,
               CI_down_Re_estimate = lower_95) %>% 
        mutate(Rt_type = Rt_value_name,
               simulation_type = simulation_type,
               noise_type = noise_type,
               method_name = method_name)
      
      all_results <- c(all_results, list(simulation_results))
    }
  }
}

all_results <- bind_rows(all_results)

results_with_ref <- full_join(reference_Rt_values, all_results, by = c("idx", "Rt_type"))

## Summarize run time
summary_runtime <- all_results %>% 
  group_by(Rt_type, replicate, simulation_type, noise_type, method_name) %>% 
  summarise(run_time = mean(run_time, na.rm = T),
            .groups = 'drop')

write_csv(summary_runtime, file = file.path(INFERENCE_DIR, "summary_runtime.csv"))


## Compute coverage, RMSE and summarize computations over all replicates
coverage_computation <- results_with_ref %>% 
  mutate(ref_value_in_CI = case_when(ref_Rt >= CI_down_Re_estimate & ref_Rt <= CI_up_Re_estimate ~ 1,
                                     is.na(ref_Rt) | is.na(CI_down_Re_estimate) | is.na(CI_up_Re_estimate) ~ NA_real_,
                                     TRUE ~ 0),
         successful_computation = case_when(is.na(Re_estimate) ~ 0,
                                            TRUE ~ 1))

summary_computation <- coverage_computation %>%
  group_by(idx, Rt_type, simulation_type, noise_type, method_name) %>% 
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
  arrange(method_name, Rt_type, simulation_type, noise_type, idx)

summary_computation$simulation_type <-  recode_factor(summary_computation$simulation_type, 
                                                      case_confirmations = "Case confirmations",
                                                      combined_low_prob = "Combined - Low prob.",
                                                      combined_high_prob = "Combined - High prob.",
                                                      combined_full_prob = "Symptom onsets")

summary_computation$Rt_type <-  recode_factor(summary_computation$Rt_type, 
                                              abrupt = "Abrupt decrease",
                                              abrupt_up = "Abrupt increase",
                                              constant = "Constant",
                                              linear_down = "Linear decrease",
                                              linear_up = "Linear increase")

write_csv(summary_computation, file = file.path(INFERENCE_DIR, "summary_inference_on_simulations.csv"))

