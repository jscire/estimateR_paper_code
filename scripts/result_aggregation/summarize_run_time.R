library(estimateR)
library(readr)
library(tidyr)
library(dplyr)
library(Metrics)
library(ggplot2)

INFERENCE_DIR <-  here::here("data", "inference_on_simulated_incidence")
SIMULATION_DIR <-  here::here("data", "simulated_incidence")


partial_obs_probabilities <- list("case_confirmations" = 0, 
                                  "combined_low_prob" = 0.3, 
                                  "combined_high_prob" = 0.6, 
                                  "combined_full_prob" = 1)

Rt_values_names <- c("abrupt", "linear_up", "linear_down", "constant", "abrupt_up")


success_rate_threshold <- 0.95

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
bootstrapping <- "bootstrapped"
noise_name <- 'iid_noise_sd'
partial_obs_name <- "case_confirmations"

for(Rt_value_name in Rt_values_names) {
  simulation_results <- try(read_csv(file =  file.path(INFERENCE_DIR, 
                                                       paste0(bootstrapping,"_inference_on_", 
                                                              partial_obs_name, "_Rt_",
                                                              Rt_value_name, "_", 
                                                              noise_name, ".csv"))),
                            silent = TRUE)
  if ("try-error" %in% class(simulation_results)) {
    # We catch errors to handle cases when simulations have not produced a successful outbreak.
    cat("File not found")
    cat(paste0(bootstrapping,"_inference_on_", 
               partial_obs_name, "_Rt_",
               Rt_value_name, "_", 
               noise_name, ".csv"))
  } else {
    simulation_results <- simulation_results %>% 
      mutate(Rt_type = Rt_value_name,
             simulation_type = partial_obs_name,
             method_name = "estimateR",
             noise_type = noise_name,
             bootstrapping_type = bootstrapping)
    
    all_results <- c(all_results, list(simulation_results))
  }
}


all_results <- bind_rows(all_results)

summary_runtime <- all_results %>% 
  group_by(Rt_type, replicate, simulation_type, noise_type, method_name) %>% 
  summarise(run_time = mean(run_time, na.rm = T),
            .groups = 'drop')

write_csv(summary_runtime, file = file.path(INFERENCE_DIR, "summary_runtime.csv"))

