library(readr)
library(dplyr)
library(ggplot2)
library(estimateR)
library(tidyr)
library(stringr)
library(lubridate)


EMPIRICAL_DATA_DIR <- here::here("data", "empirical_data")
OUT_DIR <- here::here("data", "empirical_data_results")

# Analysis config
## Delay between infection and onset of symptoms (incubation period) in days
# Gamma distribution parameter
shape_incubation <- 2.743164
scale_incubation <- 1.932075

# Incubation period delay distribution
distribution_incubation <- list(name = "gamma", 
                                shape = shape_incubation, 
                                scale = scale_incubation)


## Delay between onset of symptoms and case confirmation in days
# Gamma distribution parameter
shape_onset_to_confirmation <- 2.094875
scale_onset_to_confirmation <- 2.625455

# Incubation period delay distribution
distribution_onset_to_confirmation <- list(name = "gamma", 
                                           shape = shape_onset_to_confirmation, 
                                           scale = scale_onset_to_confirmation)

## Serial interval (for Re estimation) in days
mean_serial_interval <- 4.8
std_serial_interval <- 2.3

estimation_window = 3 # 3-day sliding window for the Re estimation
minimum_cumul_incidence = 100 # we start estimating Re after at least 100 cases have been recorded
N_bootstrap_replicates = 100 # we take 100 replicates in the bootstrapping procedure


# Analysis with estimateR

data_files <- list.files(path = EMPIRICAL_DATA_DIR, pattern = "*-Data.rds", full.names = F)

country_codes_of_interest <- c("AUS", "BEL", "CHL", "GBR", "IND", "JPN", "USA", "ZAF")
# country_codes_of_interest <- c("ESP")

for(data_file in data_files) {
  country_code <- unlist(strsplit(data_file, split = "-"))[1]
  
  if(!(country_code %in% country_codes_of_interest)) {
    next
  }
  
  empirical_data <- read_rds(file.path(EMPIRICAL_DATA_DIR, data_file)) %>% 
    filter(data_type == "Confirmed cases",
           countryIso3 == region)
  
  if(nrow(empirical_data) == 0){
    print(paste0("Skipped ", country_code))
    print("No incidence data.")
    next
  } else if (length(unique(empirical_data$date_type)) > 1) {
    print(paste0("Skipped ", country_code))
    print("Onset data available.")
    next
  } else if (nrow(empirical_data) > 700) {
    print(paste0("Skipped ", country_code))
    print("Too many dates available.")
    next
  }
  
  print(country_code)
  
  empirical_incidence <- empirical_data %>% 
    transmute(date = date,
              case_incidence = value) %>% 
    replace_na(list(case_incidence = 0)) %>% 
    complete(date = seq.Date(min(date),  max(date),
                             by = "days"),
             fill = list(case_incidence = 0))
  
  
  # We specifiy the reference date (first date of data) and the time step of data.
  ref_date = min(empirical_incidence$date)
  time_step = "day"
  
  Re_estimates <- try(get_block_bootstrapped_estimate(
    incidence_data = empirical_incidence$case_incidence,
    N_bootstrap_replicates = N_bootstrap_replicates,
    delay = list(distribution_incubation,  distribution_onset_to_confirmation),
    estimation_window = estimation_window,
    minimum_cumul_incidence = minimum_cumul_incidence,
    mean_serial_interval = mean_serial_interval,
    std_serial_interval = std_serial_interval,
    ref_date = ref_date,
    time_step = time_step,
    combine_bootstrap_and_estimation_uncertainties = TRUE,
    output_Re_only = TRUE
  ), silent = TRUE)
  
  if ("try-error" %in% class(Re_estimates)) {
    print(paste0(country_code, "Error in estimation."))
    next
  } 
  
  estimateR_results <- Re_estimates %>% 
    mutate(implementation = "estimateR")
  
  pipeline_results <- try(read_rds(file = file.path(EMPIRICAL_DATA_DIR, paste0(country_code, "-Estimates.rds"))), silent = TRUE)
  if ("try-error" %in% class(pipeline_results)) {
    print(paste0(country_code, "No pipeline estimates."))
    next
  }
  
  pipeline_results <- pipeline_results %>% 
    filter(estimate_type == "Cori_slidingWindow", 
           data_type == "Confirmed cases",
           countryIso3 == region) %>% 
    transmute(date = date,
              Re_estimate = median_R_mean,
              CI_up_Re_estimate = median_R_highHPD,
              CI_down_Re_estimate = median_R_lowHPD,
              implementation = "pipeline")
  
  results_for_comparison <- bind_rows(pipeline_results, estimateR_results)
  
  write_csv(results_for_comparison, file = file.path(OUT_DIR, paste0(country_code, "_compared_estimates.csv")))
}


