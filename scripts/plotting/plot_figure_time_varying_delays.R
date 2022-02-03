library(estimateR)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Metrics)
library(viridis)
library(cowplot)

INFERENCE_DIR <-  here::here("data", "inference_on_simulated_incidence")
PLOT_DIR <- here::here("plots")

summarized_results <- read_csv(file = file.path(INFERENCE_DIR, "summary_inference_on_simulations_time_varying_delays.csv"), 
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

all_figure_data <- summarized_results

all_figure_data$analysis_delay <-  recode_factor(all_figure_data$analysis_delay, 
                                                     short = "Short",
                                                     long = "Long",
                                                     short_to_long = "Short to long",
                                                     long_to_short = "Long to short")

main_figure_data <- filter(all_figure_data, 
                           noise_type == "autocorrelated",
                           bootstrapping_type == "bootstrapped")

### Build main figure
colour_palette <- viridis(7)

theme_set(theme_bw() +
            theme(
              strip.text = element_text(size = 24),
              axis.title = element_text(size = 28),
              panel.spacing = unit(0.9, "lines")
            ))

p1 <- ggplot(filter(main_figure_data, delay_change_type == "long_to_short"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay) + 
  geom_line(aes(y = ref_Rt), lwd =  1.1, colour = "black") +
  geom_line(aes(y = median_Re_estimate), lwd =  1.1,  colour = colour_palette[5]) +
  geom_ribbon(aes(x = idx, ymax = median_CI_up_Re_estimate, ymin = median_CI_down_Re_estimate),
              alpha = 0.45,  fill = colour_palette[5]) +
  ylab("Reproductive number") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 3.5),
                  xlim = c(0, 150)) +
  scale_colour_viridis_d() +
  theme(strip.background = element_rect(colour="black",
                                  fill="white"),
        strip.text.y = element_blank(),
        axis.text = element_text(size = 20),
        plot.margin = margin(5, 10, 10, 30))

p2 <- ggplot(filter(main_figure_data, delay_change_type == "short_to_long"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay, labeller = label_wrap_gen(width = 14)) + 
  geom_line(aes(y = ref_Rt), lwd =  1.1,  colour = "black") +
  geom_line(aes(y = median_Re_estimate), lwd =  1.1,  colour = colour_palette[5]) +
  geom_ribbon(aes(x = idx, ymax = median_CI_up_Re_estimate, ymin = median_CI_down_Re_estimate),
              alpha = 0.45,  fill = colour_palette[5]) +
  ylab("Reproductive number") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 3.5),
                  xlim = c(0, 150)) +
  scale_colour_viridis_d() +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text = element_text(size = 20),
        plot.margin = margin(5, 5, 10, 20))

plot_grid(p1, p2, 
          labels = c('A', 'B'), 
          label_size = 45)

ggsave(filename = file.path(PLOT_DIR, "Figure_time_varying_delays_main.png"), dpi = 320, width = 50, height = 24, units = "cm")


### Build SI figure (coverage and RMSE)
p1 <- ggplot(filter(main_figure_data, delay_change_type == "long_to_short"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay) + 
  geom_line(aes(y = coverage), lwd =  1.1, colour = "black") +
  ylab("Coverage") +
  xlab("") +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 150)) +
  theme(strip.text.y = element_blank(),
        strip.background.x = element_rect(colour="black",
                                          fill="white"),
        axis.text = element_text(size = 16),
        plot.margin = margin(5, 5, 5, 30))

p2 <- ggplot(filter(main_figure_data, delay_change_type == "short_to_long"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay, labeller = label_wrap_gen(width = 14)) +
  geom_line(aes(y = coverage), lwd =  1.1,  colour = "black") +
  ylab("Coverage") +
  xlab("") +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 150)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text = element_text(size = 16),
        plot.margin = margin(5, 5, 5, 30))

p3 <- ggplot(filter(main_figure_data, delay_change_type == "long_to_short"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay, labeller = label_wrap_gen(width = 14)) + 
  geom_line(aes(y = rmse), lwd =  1.1, colour = "black") +
  ylab("RMSE") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 1.2),
                  xlim = c(0, 150)) +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        strip.background.x = element_rect(colour="black",
                                          fill="white"),
        axis.text = element_text(size = 16),
        plot.margin = margin(5, 5, 10, 30))

p4 <- ggplot(filter(main_figure_data, delay_change_type == "short_to_long"), aes(x = idx)) +
  facet_grid(Rt_type ~ analysis_delay, labeller = label_wrap_gen(width = 14)) +
  geom_line(aes(y = rmse), lwd =  1.1,  colour = "black") +
  ylab("RMSE") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 1.2),
                  xlim = c(0, 150)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        strip.text.x = element_blank(),
        axis.text = element_text(size = 16),
        plot.margin = margin(5, 5, 10, 30))

pSI <- plot_grid(p1, p2, p3, p4,
          labels = c("A", "B", "C", "D"),
          label_size = 45,
          nrow=2)
ggsave(plot = pSI, filename = file.path(PLOT_DIR, "Figure_time_varying_delays_SI.png"), dpi = 320, width = 50, height = 45, units = "cm")
