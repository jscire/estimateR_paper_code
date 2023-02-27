#!/usr/bin/env Rscript

library(epidemia)
library(lubridate)
library(rstanarm)
library(readr)
library(dplyr)
library(optparse)
library(coda)
library(tictoc)


DATA_DIR <- here::here("data", "simulated_incidence")
OUT_DIR <- here::here("data", "epidemia")

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
    opt$noise <- "iid_noise_sd"
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

# Detect available cores to parallelize computations
options(mc.cores = parallel::detectCores())

# Config
## Serial interval (for Re estimation) in days
mean_serial_interval <- 4.8
std_serial_interval <- 2.3

# Incubation period - gamma distribution parameters
shape_incubation <- 3.2
scale_incubation <- 2.1
incubation <- list(name="gamma", shape = shape_incubation, scale = scale_incubation)

# Delay from onset of symptoms to case observation - gamma distribution parameters
shape_onset_to_report <- 2.7
scale_onset_to_report <- 2.6
onset_to_report <- list(name="gamma", shape = shape_onset_to_report, scale = scale_onset_to_report)

observation_delay <- estimateR::convolve_delays(list(incubation, onset_to_report), max_quantile = 0.999999)
serial_interval <- estimateR:::.get_infectiousness_profile(mean_SI = mean_serial_interval, sd_SI = std_serial_interval, 1)

# Forced to do this by epiinf (error cast if distribution does not exactly sum to one)
serial_interval[2] <- serial_interval[2] + (1 - sum(serial_interval))
observation_delay[which.max(observation_delay)] <- max(observation_delay) + (1- sum(observation_delay))

noise_name <- opt$noise
Rt_value_name <- opt$Rt
partial_obs_name <- opt$partial_obs_prob
method_name <- "epidemia"

simulated_incidence_data <- read_csv(file = file.path(DATA_DIR, paste0(partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, ".csv")),
                                     col_types = list(
                                       partially_delayed = col_double(),
                                       fully_delayed = col_double(),
                                       replicate = col_integer()))


date_start <- as.Date("2020-01-01")
all_reported_cases <- simulated_incidence_data %>% 
  select(-partially_delayed) %>% 
  rename(cases = fully_delayed) %>% 
  group_by(replicate) %>% 
  mutate(date = date_start + row_number() - 1 ) %>% 
  ungroup()

number_of_replicates <- 100
start_num_replicates <- 1

for(replicate_id in start_num_replicates:number_of_replicates) {
  
  print(replicate_id)
  
  data <- all_reported_cases %>% 
    filter(replicate == replicate_id) %>% 
    select(date, cases) %>% 
    mutate(cases = if_else(date == date_start, NA_real_,cases),
           city = "made_up")
  
  inf <- epiinf(gen = serial_interval)
  
  obs <-  epiobs(formula = cases ~ 0 + offset(rep(1,150)),
                 link = "identity",
                 family = "neg_binom",
                 i2o = observation_delay)
  
  rt <- epirt(formula = R(city, date) ~ 1 + rw(prior_scale = 0.2),
              link = 'log')
  
  args <- list(
    # algorithm = "meanfield",
    algorithm = "sampling", # set to 'sampling' when final analysis
    rt = rt, 
    obs = obs, 
    inf = inf, 
    data = data, 
    iter = 2e3)
  
  start_time <- Sys.time()
  inference_result <- do.call(epim, args)
  end_time <- Sys.time()
  
  run_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  posterior_rt_samples <- mcmc(posterior_rt(inference_result)$draws)
  hpd_interval <- HPDinterval(posterior_rt_samples)
  
  Re_estimates <- apply(posterior_rt_samples, MARGIN = 2, FUN = mean)
  
  computation_results <- tibble(idx = 1:nrow(data), 
         Re_estimate = Re_estimates, 
         CI_up_Re_estimate = hpd_interval[,"upper"],
         CI_down_Re_estimate = hpd_interval[,"lower"]) %>% 
    mutate(replicate = replicate_id,
           run_time = run_time) %>% 
    mutate(Rt_type = Rt_value_name,
           simulation_type = partial_obs_name,
           noise_type = noise_name,
           method_name = method_name)
  
  write_csv(computation_results, file = file.path(OUT_DIR, paste0("epidemia_inference_on_", partial_obs_name, "_Rt_",Rt_value_name, "_", noise_name, "_replicate_", replicate_id, ".csv")))
}