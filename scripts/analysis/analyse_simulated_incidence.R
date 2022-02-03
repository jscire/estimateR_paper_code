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
              help="noise model name", metavar="character"),
  make_option(c("-s", "--SI"), type="boolean", default=FALSE,
              help="is analysis for supplementary: LOESS smoothing of Poisson-only noise", metavar="character")
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
  if(is.null(opt$SI)) {
    opt$SI <- FALSE
    cat("Setting SI to default value.\n")
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

# Delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2.7
scale_onset_to_report <- 2.6
onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

noise_name <- opt$noise
Rt_value_name <- opt$Rt
partial_obs_name <- opt$partial_obs_prob
is_SI_analysis <- !opt$SI

# Assign a smoothing method to each noise model
smoothing_method <- list(noiseless = ifelse(is_SI_analysis, "LOESS", "none"),
                         iid_noise_sd = "LOESS",
                         autocorrelated = "LOESS")



# Read simulated data
simulated_incidence_data <- read_csv(file = file.path(DATA_DIR, paste0(partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")),
                                     col_types = list(
                                       partially_delayed = col_double(),
                                       fully_delayed = col_double(),
                                       replicate = col_integer()))

all_bootstrapped_results <- list()

for(replicate_id in unique(simulated_incidence_data$replicate)) {
  incidence_data <- filter(simulated_incidence_data, replicate == replicate_id)
  
  start_time <- Sys.time()
  # Use estimateR function to estimate Re through time
  result_inference <- try(get_bootstrapped_estimates_from_combined_observations(
    partially_delayed_incidence = incidence_data$partially_delayed,
    fully_delayed_incidence = incidence_data$fully_delayed,
    smoothing_method = smoothing_method[[noise_name]],
    deconvolution_method = deconvolution_method,
    estimation_method = estimation_method,
    uncertainty_summary_method = uncertainty_summary_method,
    N_bootstrap_replicates = N_bootstrap_replicates,
    delay_until_partial = incubation,
    delay_until_final_report = onset_to_report,
    partial_observation_requires_full_observation = TRUE,
    combine_bootstrap_and_estimation_uncertainties = combine_bootstrap_and_estimation_uncertainties,
    estimation_window = estimation_window,
    minimum_cumul_incidence = minimum_cumul_incidence,
    mean_serial_interval = mean_serial_interval,
    std_serial_interval = std_serial_interval,
    output_Re_only = FALSE,
    data_points_incl = data_points_incl
  ), silent = TRUE)
  end_time <- Sys.time()
  run_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if ("try-error" %in% class(result_inference)) {
    # We catch errors to handle cases when simulations have not produced a successful outbreak.
    print(result_inference)
  } else {
    result_inference <- result_inference %>%
      mutate(replicate = replicate_id,
             run_time = run_time) #Keep track of run time of each analysis
    
    all_bootstrapped_results <- c(all_bootstrapped_results, list(result_inference))
  }
}

all_bootstrapped_results <- bind_rows(all_bootstrapped_results)
write_csv(all_bootstrapped_results, file = file.path(OUT_DIR, paste0(ifelse(is_SI_analysis, "SI_", ""), "bootstrapped_inference_on_", partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")))
