library(estimateR)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Metrics)
library(viridis)
library(cowplot)

ESTIMATER_DIR <-  here::here("data", "inference_on_simulated_incidence")
EPIDEMIA_DIR <-  here::here("data", "epidemia")
EPINOW2_DIR <-  here::here("data", "epinow2")
PLOT_DIR <- here::here("plots")



## Get results from each package
summary_file_name <- "summary_inference_on_simulations.csv"

# estimateR
estimateR_summarized_results <- read_csv(file = file.path(ESTIMATER_DIR, summary_file_name), 
                               col_types = list(
  idx = col_double(),
  Rt_type = col_character(),
  simulation_type = col_character(),
  noise_type = col_character(),
  bootstrapping_type = col_character(),
  rmse = col_double(),
  coverage = col_double(),
  median_Re_estimate = col_double(),
  median_CI_down_Re_estimate = col_double(),
  median_CI_up_Re_estimate = col_double(),
  success_rate = col_double(),
  ref_Rt = col_double()
))

estimateR_results_figure_data <- filter(estimateR_summarized_results, 
                                simulation_type == "Case confirmations",
                                noise_type == "iid_noise_sd",
                                bootstrapping_type == "bootstrapped") %>% 
                        mutate(method_name = "estimateR") %>% 
                        select(-bootstrapping_type)

# epidemia
epidemia_summarized_results <- read_csv(file = file.path(EPIDEMIA_DIR, summary_file_name), 
                                         col_types = list(
                                           idx = col_double(),
                                           Rt_type = col_character(),
                                           simulation_type = col_character(),
                                           noise_type = col_character(),
                                           method_name = col_character(),
                                           rmse = col_double(),
                                           coverage = col_double(),
                                           median_Re_estimate = col_double(),
                                           median_CI_down_Re_estimate = col_double(),
                                           median_CI_up_Re_estimate = col_double(),
                                           success_rate = col_double(),
                                           ref_Rt = col_double()
                                         ))

epidemia_results_figure_data <- epidemia_summarized_results

# EpiNow2
epinow2_summarized_results <- read_csv(file = file.path(EPINOW2_DIR, summary_file_name), 
                                        col_types = list(
                                          idx = col_double(),
                                          Rt_type = col_character(),
                                          simulation_type = col_character(),
                                          noise_type = col_character(),
                                          method_name = col_character(),
                                          rmse = col_double(),
                                          coverage = col_double(),
                                          median_Re_estimate = col_double(),
                                          median_CI_down_Re_estimate = col_double(),
                                          median_CI_up_Re_estimate = col_double(),
                                          success_rate = col_double(),
                                          ref_Rt = col_double()
                                        ))

epinow2_results_figure_data <- epinow2_summarized_results

# Gather results
results_figure_data <- bind_rows(estimateR_results_figure_data, epidemia_results_figure_data, epinow2_results_figure_data) %>% 
  filter(!is.na(method_name))

results_figure_data$method_name <- recode_factor(results_figure_data$method_name,
                                         estimateR = "estimateR",
                                         epinow2 = "EpiNow2",
                                         epidemia  = "epidemia")


# Make main figure panel A
colour_palette <- turbo(9)

theme_set(theme_bw() +
            theme(
              axis.title = element_text(size = 28),
              axis.text = element_text(size = 20),
              panel.spacing.y = unit(0.9, "lines"),
              legend.text = element_text(size = 20),
              legend.title = element_text(size = 24)
            ))

pA <- ggplot(results_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .,  labeller = label_wrap_gen(width = 10)) +
  geom_line(data = filter(results_figure_data, method_name == "estimateR"), aes(y = ref_Rt), lwd =  0.9, colour = "black") +
  geom_line(aes(y = median_Re_estimate, group = method_name, colour = method_name), lwd =  1.6) +
  geom_ribbon(aes(x = idx, ymax = median_CI_up_Re_estimate, ymin = median_CI_down_Re_estimate, group = method_name, fill = method_name),
              alpha = 0.2) +
  scale_colour_viridis_d(name = "R package:", option = "B", begin = 0.25, end= 0.85) +
  scale_fill_viridis_d(name = "R package:", option = "B",  begin = 0.25, end= 0.85) +
  ylab("Reproductive number") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 3.5),
                  xlim = c(0, 150)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        strip.text = element_text(size = 24),
        plot.margin = margin(12, 10, 10, 30),
        legend.position="bottom")

#### Prepare panel B (run time comparison)

summary_file_name <- "summary_runtime.csv"

# estimateR
estimateR_runtime <- read_csv(file = file.path(ESTIMATER_DIR, summary_file_name),
                              col_types = list(
                                Rt_type = col_character(),
                                simulation_type = col_character(),
                                noise_type = col_character(),
                                method_name = col_character(),
                                run_time = col_double()
                              ))
# epidemia
epidemia_runtime <- read_csv(file = file.path(EPIDEMIA_DIR, summary_file_name),
                             col_types = list(
                               Rt_type = col_character(),
                               simulation_type = col_character(),
                               noise_type = col_character(),
                               method_name = col_character(),
                               run_time = col_double()
                             ))

# epinow2
epinow2_runtime <- read_csv(file = file.path(EPINOW2_DIR, summary_file_name),
                            col_types = list(
                              Rt_type = col_character(),
                              simulation_type = col_character(),
                              noise_type = col_character(),
                              method_name = col_character(),
                              run_time = col_double()
                            ))

# Gather results
runtime_data <- bind_rows(estimateR_runtime, epidemia_runtime, epinow2_runtime) %>% 
  filter(!is.na(method_name))

runtime_data$method_name <- recode_factor(runtime_data$method_name,
                                         estimateR = "estimateR",
                                         epinow2 = "EpiNow2",
                                         epidemia  = "epidemia")

# Prepare panel
pB <- ggplot(runtime_data) +
  geom_boxplot(aes(y = run_time, fill = method_name), lwd = 0.3) +
  scale_fill_viridis_d(name = "R package:", option = "B",  begin = 0.25, end= 0.85) +
  scale_y_log10(limits = c(1, 10000)) +
  ylab("Time (s)") +
  xlab("") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(12, 10, 10, 25),
    legend.position = c(0.7, 0.2)
  )

###### Draw full main text figure
prow <- plot_grid(pA + theme(legend.position="none"), 
                  pB + theme(legend.position="none"),
                  labels = c("A", "B"),
                  label_size = 45,
                  nrow=1)

legend_b <- get_legend(pA + theme(legend.position="bottom"))

p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .04))

p

ggsave(filename = file.path(PLOT_DIR, "Figure_main_method_comparison.png"), dpi = 320, height = 30, width = 40, units="cm")



### Prepare SI figure with coverage and RMSE


figure_data <- results_figure_data
p1 <- ggplot(figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .) +
  geom_line(aes(y = coverage, group = method_name, colour = method_name), lwd =  1.1) +
  ylab("Coverage") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 150)) +
  scale_colour_viridis_d(name = "R package:", option = "B", begin = 0.25, end= 0.85) +
  theme(strip.text = element_blank(),
        plot.margin =margin(12, 5, 10, 30),
        legend.position = "none")

p2 <- ggplot(figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ ., labeller = label_wrap_gen(width = 10)) +
  geom_line(aes(y = rmse, group = method_name, colour = method_name), lwd =  1.1) +
  ylab("RMSE") +
  xlab("Time") +
  scale_colour_viridis_d(name = "R package:", option = "B", begin = 0.25, end= 0.85) +
  coord_cartesian(ylim = c(0, 1.5),
                  xlim = c(0, 150)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        strip.text = element_text(size = 24),
        plot.margin = margin(12, 10, 10, 30),
        legend.position = "none")

legend <- get_legend(p2 + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

combined_plots <- plot_grid(p1,p2,
                            labels = c("A", "B"),
                            label_size = 45,
                            ncol = 2)

plot_grid(combined_plots,
          legend,
          ncol = 1,
          rel_heights = c(1, .035))

ggsave(filename = file.path(PLOT_DIR, "Figure_SI_method_comparison.png"), dpi = 320, height = 45, width = 50, units="cm")

