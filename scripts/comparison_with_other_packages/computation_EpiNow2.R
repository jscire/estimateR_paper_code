#!/usr/bin/env Rscript

library(EpiNow2)
library(readr)
library(dplyr)
library(optparse)
library(lubridate)


DATA_DIR <- here::here("data", "simulated_incidence")
OUT_DIR <- here::here("data", "epinow2")

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
    cat("Setting Rt to default value.\n")
  }
  if(is.null(opt$noise)) {
    opt$noise <- "autocorrelated"
    # opt$noise <- "noiseless"
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

# Incubation period - gamma distribution parameters
shape_incubation <- 3.2
scale_incubation <- 2.1

# Delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2.7
scale_onset_to_report <- 2.6

incubation_delay <- estimate_delay(rgamma(n=1E3, shape = shape_incubation, scale = scale_incubation), max_value = 40, bootstraps = 1)
reporting_delay <- estimate_delay(rgamma(n=1E3, shape = shape_onset_to_report, scale = scale_onset_to_report), max_value = 40, bootstraps = 1)
generation_time <- list(mean = mean_serial_interval, sd = std_serial_interval,
                        mean_sd = 0.1, sd_sd = 0.1, max_value = 30)

noise_name <- opt$noise
Rt_value_name <- opt$Rt
partial_obs_name <- opt$partial_obs_prob

simulated_incidence_data <- read_csv(file = file.path(DATA_DIR, paste0(partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")),
                                     col_types = list(
                                       partially_delayed = col_double(),
                                       fully_delayed = col_double(),
                                       replicate = col_integer()))


date_start <- as.Date("2020-01-01")
all_reported_cases <- simulated_incidence_data %>% 
  select(-partially_delayed) %>% 
  rename(confirm = fully_delayed) %>% 
  group_by(replicate) %>% 
  mutate(date = date_start + row_number() - 1 ) %>% 
  ungroup()

num_start_replicate <- 1
number_of_replicates <- 100

for(replicate_id in num_start_replicate:number_of_replicates) {
  print(replicate_id)
  
  reported_cases <- all_reported_cases %>% 
    filter(replicate == replicate_id) %>% 
    select(date, confirm)
  
  start_time <- Sys.time()
  estimates <- epinow(reported_cases = reported_cases,
                      generation_time = generation_time,
                      delays = delay_opts(incubation_delay, reporting_delay),
                      horizon = 0,
                      return_output = TRUE,
                      target_folder=  here::here("data", "epinow2", "output_data"),
                      logs =  here::here("data", "epinow2", "log_data"),
                      stan = stan_opts(cores = 4,
                                       control = list(adapt_delta = 0.96,
                                                      max_treedepth = 16)),
                      rt = rt_opts(gp_on = "R0"), 
                      obs = obs_opts(family = "negbin",
                                     week_effect = FALSE),
                      id = replicate_id,
                      CrIs = c(0.5, 0.95))
  
  end_time <- Sys.time()
  run_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  R_values <- summary(estimates, type = "parameters", params = "R") %>% 
    mutate(replicate = replicate_id,
           run_time = run_time)
  write_csv(R_values, file = file.path(OUT_DIR, paste0("epinow2_inference_on_", partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, "_replicate_", replicate_id, ".csv")))
  print(paste0("Timing: ", run_time/3600, " hours."))
}