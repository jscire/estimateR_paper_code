library(estimateR)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)
library(Metrics)

OUT_DIR <- here::here("data", "simulated_incidence")
PLOT_DIR <- here::here("plots")

Rt_values_abrupt_up <- smooth_incidence(c(rep(1.25, times = 133), rep(3, times = 17)) , data_points_incl = 8)

Rt_values <- Rt_values_abrupt_up
imported_infections <- c(10)

delay_long <- list(name = "gamma", shape = 2, scale = 2)
delay_short <- list(name = "gamma", shape = 2, scale = 2)
delay <- list(delay_short, delay_long)

delay_median <- which(cumsum(convolve_delays(delays= delay)) > 0.5)[1]

noise <- list(type = 'autocorrelated', ar_coeffs = c(0.05, 0.05, -0.02, -0.2), sd = 0.05)



# Simulate NUM_REPLICATES replicates
NUM_REPLICATES <- 100

simulations <- c()
for(i in 1:NUM_REPLICATES){
  # Simulate infections
  infects <- simulate_infections(Rt_values, imported_infections)
  simulated_observations <- simulate_delayed_observations(infects, 
                                                          delay = list(delay_short, delay_long),
                                                          noise = noise)
  
  simulated_observations <- data.frame(incidence = simulated_observations,
                                       replicate = i)
  simulations <- c(simulations, list(simulated_observations))
}
simulations <- bind_rows(simulations)




## Analyze all simulated replicates
all_bootstrapped_results <- list()
for(i in 1:NUM_REPLICATES){
  print(i)
  simulated_observations <- filter(simulations, replicate == i)
  
  Re_with_new_rightpadding <- get_block_bootstrapped_estimate(
    incidence_data = simulated_observations$incidence,
    delay = list(delay_short, delay_long),
    N_bootstrap_replicates  = 100,
    constant_right_padding = FALSE,
    combine_bootstrap_and_estimation_uncertainties = TRUE,
    output_Re_only = FALSE,
    include_index = TRUE,
    data_points_incl = 9) %>% 
    mutate(Rt_type = "new_rightpadding") 
  
  Re_with_old_rightpadding <- get_block_bootstrapped_estimate(
    incidence_data = simulated_observations$incidence,
    delay = list(delay_short, delay_long),
    N_bootstrap_replicates  = 100,
    constant_right_padding = TRUE,
    combine_bootstrap_and_estimation_uncertainties = TRUE,
    output_Re_only = FALSE,
    include_index = TRUE,
    data_points_incl = 9) %>% 
    mutate(Rt_type = "old_rightpadding") 
  
  comparison_data <- bind_rows(Re_with_new_rightpadding, Re_with_old_rightpadding) %>% 
    select(idx, Re_estimate, CI_down_Re_estimate, CI_up_Re_estimate, Rt_type) %>% 
    mutate(replicate = i) 
  
  all_bootstrapped_results <- c(all_bootstrapped_results, list(comparison_data))
}
all_bootstrapped_results <- bind_rows(all_bootstrapped_results)



## Summarize results over all replicates
reference_Rt_values <- data.frame(idx = seq_along(Rt_values), ref_Rt = Rt_values)
results_with_ref <- full_join(reference_Rt_values, all_bootstrapped_results, by = "idx")

coverage_computation <- results_with_ref %>% 
  filter(!is.na(Rt_type)) %>% 
  mutate(ref_value_in_CI = case_when(ref_Rt >= CI_down_Re_estimate & ref_Rt <= CI_up_Re_estimate ~ 1,
                                     is.na(ref_Rt) | is.na(CI_down_Re_estimate) | is.na(CI_up_Re_estimate) ~ NA_real_,
                                     TRUE ~ 0),
         successful_computation = case_when(is.na(Re_estimate) ~ 0,
                                            TRUE ~ 1))

success_rate_threshold <- 0.95
summary_computation <- coverage_computation %>% 
  group_by(idx, Rt_type) %>% 
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
  arrange(Rt_type, idx)

plot_comparison_data <- summary_computation

plot_comparison_data$`Re values` <- recode_factor(plot_comparison_data$Rt_type,
                                                  new_rightpadding= "estimateR",
                                                  old_rightpadding= "Huisman et al. pipeline")

write_csv(plot_comparison_data, file = file.path(OUT_DIR, "plot_data_rightpadding_old_vs_new_autocorrelated.csv"))

### Build figure
plot_comparison_data <- read_csv(file = file.path(OUT_DIR, "plot_data_rightpadding_old_vs_new_autocorrelated.csv"),
                                 col_types = list(
                                   idx = col_double(),
                                   Rt_type = col_character(),
                                   rmse = col_double(),
                                   coverage = col_double(),
                                   median_Re_estimate = col_double(),
                                   median_CI_down_Re_estimate = col_double(),
                                   median_CI_up_Re_estimate = col_double(),
                                   success_rate = col_double(),
                                   ref_Rt = col_double(),
                                   `Re values` = col_character()
                                 ))

colour_palette <- viridis(7)
ggplot(filter(plot_comparison_data, between(idx, 120, 150)), aes(x = idx)) +
  geom_line(aes(y = ref_Rt), lwd =  1.3, color = "black") +
  geom_line(aes(y = median_Re_estimate, color = `Re values`, group = `Re values`), lwd = 1.1) + 
  geom_ribbon(aes(x = idx, ymax = median_CI_up_Re_estimate, ymin = median_CI_down_Re_estimate, fill = `Re values`, group = `Re values`),
              alpha = 0.25) +
  coord_cartesian(xlim = c(134,143),
                  ylim  = c(2, 3.8)) +
  scale_colour_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() +
  ylab("Reproductive number") +
  xlab("Time (arbitrary units)") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = "none")

ggsave(filename = file.path(PLOT_DIR, "Re_rightpadding_old_vs_new.png"), width = 12, height = 9, units = "cm", dpi = 320)
