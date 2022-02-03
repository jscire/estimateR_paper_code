#!/usr/bin/env Rscript

library(estimateR)
library(readr)
library(dplyr)
library(optparse)


DATA_DIR <- here::here("data", "simulated_incidence")
OUT_DIR <- here::here("data", "inference_on_simulated_incidence")

option_list = list(
  make_option(c("-p", "--partial_obs_prob"), type="character", default=NULL,
              help="Name of partial observation probability", metavar="character"),
  make_option(c("-r", "--Rt"), type="character", default=NULL,
              help="Rt series name", metavar="character"),
  make_option(c("-n", "--noise"), type="character", default=NULL,
              help="noise model name", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# Set default values for manual execution and testing
if(interactive()) {
  if(is.null(opt$partial_obs_prob)) {
    opt$partial_obs_prob <- "case_confirmations"
    cat("Setting partial_obs_prob to default value.\n")
  }
  if(is.null(opt$Rt)) {
    opt$Rt <- "abrupt_up"
    # opt$Rt <- "linear_down"
    cat("Setting Rt to default value.\n")
  }
  if(is.null(opt$noise)) {
    opt$noise <- "autocorrelated"
    cat("Setting noise to default value.\n")
  }
  
  # If not interactive session, throw error if missing argument.
} else {
  if (any(c(is.null(opt$partial_obs_prob), is.null(opt$Rt), is.null(opt$noise)))){
    print_help(opt_parser)
    stop("Missing argument.
       All three arguments (partial_obs_prob, Rt, noise) must be provided.n", call.=FALSE)
  }
}

# Config
## Serial interval (for Re estimation) in days
mean_serial_interval <- 4.8
std_serial_interval <- 2.3

## Methods used for calculations
deconvolution_method <- "Richardson-Lucy delay distribution"
estimation_method <- "EpiEstim sliding window"
uncertainty_summary_method <- "original estimate - CI from bootstrap estimates"


# smoothing parameters
data_points_incl <- 9

## Re estimation parameters
minimum_cumul_incidence <- 50
estimation_window <- 3

## Uncertainty estimation parameters
N_bootstrap_replicates <- 100
combine_bootstrap_and_estimation_uncertainties <- TRUE

# Incubation period - gamma distribution parameters
shape_incubation <- 3.2
scale_incubation <- 2.1
incubation <- list(name="gamma", shape = shape_incubation, scale = scale_incubation)

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
                                                                                                                       n_time_steps = 150))

long_to_short_delay_matrix <- estimateR:::.get_delay_distribution(estimateR:::.build_list_of_gradually_changing_delays(init_delay = long_onset_to_report,
                                                                                                                       final_delay= short_onset_to_report,
                                                                                                                       n_time_steps = 150))

delay_changes <- list("short_to_long" = short_to_long_delay_matrix,
                      "long_to_short" = long_to_short_delay_matrix)

smoothing_method <- list(noiseless = "none",
                         iid_noise_sd = "LOESS",
                         autocorrelated = "LOESS")

noise_name <- opt$noise
Rt_value_name <- opt$Rt
partial_obs_name <- "case_confirmations"
delay_variation_name <- opt$delay_variation

simulated_incidence_data <- read_csv(file = file.path(DATA_DIR, paste0(delay_variation_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")),
                                     col_types = list(
                                       partially_delayed = col_double(),
                                       fully_delayed = col_double(),
                                       replicate = col_integer()))



analysis_delays <- list("short" = short_onset_to_report,
                        "long" = long_onset_to_report)

analysis_delays[[delay_variation_name]] <- delay_changes[[delay_variation_name]]

for(delay_name in names(analysis_delays)) {
  all_bootstrapped_results <- list()
  print(delay_name)
  for(replicate_id in unique(simulated_incidence_data$replicate)) {
    print(replicate_id)
    incidence_data <- filter(simulated_incidence_data, replicate == replicate_id)
    
    result_inference <- try(get_block_bootstrapped_estimate(
      incidence_data = incidence_data$fully_delayed,
      smoothing_method = smoothing_method[[noise_name]],
      deconvolution_method = deconvolution_method,
      estimation_method = estimation_method,
      uncertainty_summary_method = uncertainty_summary_method,
      N_bootstrap_replicates = N_bootstrap_replicates,
      delay = list(incubation, analysis_delays[[delay_name]]),
      combine_bootstrap_and_estimation_uncertainties = combine_bootstrap_and_estimation_uncertainties,
      estimation_window = estimation_window,
      minimum_cumul_incidence = minimum_cumul_incidence,
      mean_serial_interval = mean_serial_interval,
      std_serial_interval = std_serial_interval,
      output_Re_only = FALSE,
      data_points_incl = data_points_incl
    ), silent = TRUE)
    
    if ("try-error" %in% class(result_inference)) {
      # We catch errors to handle cases when simulations have not produced a successful outbreak.
      print(result_inference)
    } else {
      result_inference <- result_inference %>%
        mutate(replicate = replicate_id)
      
      all_bootstrapped_results <- c(all_bootstrapped_results, list(result_inference))
    }
  }
  
  all_bootstrapped_results <- bind_rows(all_bootstrapped_results)
  write_csv(all_bootstrapped_results, file = file.path(OUT_DIR, paste0("bootstrapped_inference_on_", delay_variation_name, "_delay_", delay_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")))
}

