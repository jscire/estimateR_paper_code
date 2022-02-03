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

# Read stored results
summarized_results <- read_csv(file = file.path(INFERENCE_DIR, "summary_inference_on_simulations.csv"), 
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

all_figure_data <- filter(summarized_results, simulation_type == "Case confirmations")
main_figure_data <- filter(all_figure_data, 
                           noise_type %in% c("noiseless", "autocorrelated"),
                           bootstrapping_type == "bootstrapped")

# Recode factors for better plotting
main_figure_data$noise_type <- recode_factor(main_figure_data$noise_type,
                                             noiseless = "No noise",
                                             autocorrelated = "Autocorrelated")

# Noisy figure (Main text)
colour_palette <- viridis(7)

theme_set(theme_bw() +
            theme(
              axis.title = element_text(size = 28),
              axis.text = element_text(size = 20),
              panel.spacing.y = unit(0.9, "lines"),
              legend.title = element_text(size = 26),
              legend.text = element_text(size = 24)
            ))

# Build figure panel by panel
p1 <- ggplot(main_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .) +
  geom_line(aes(y = ref_Rt), lwd =  1.1, colour = "black") +
  geom_line(aes(y = median_Re_estimate, 
                group = noise_type, 
                colour = noise_type), lwd =  1.3) +
  geom_ribbon(aes(x = idx, 
                  ymax = median_CI_up_Re_estimate, 
                  ymin = median_CI_down_Re_estimate, 
                  group = noise_type,
                  # colour = noise_type,
                  fill = noise_type),
              alpha = 0.25) +
  scale_discrete_manual(values = colour_palette[c(3,5)],
                        aesthetics = c("colour", "fill"),
                        name = "Noise model:") +
  ylab("Reproductive number") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 3),
                  xlim = c(0, 150)) +
  theme(strip.text = element_blank(),
        plot.margin = margin(12, 5, 0, 20))

p2 <- ggplot(main_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .) +
  geom_line(aes(y = coverage,
                group = noise_type, 
                colour = noise_type), lwd =  1.3) +
  ylab("Coverage") +
  xlab("Time") +
  scale_discrete_manual(values = colour_palette[c(3,5)],
                        aesthetics = c("colour", "fill"),
                        name = "Noise model") +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 150)) +
  theme(strip.text = element_blank(),
        plot.margin =margin(17, 5, 0, 5))

p3 <- ggplot(main_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ ., labeller = label_wrap_gen(width = 10)) +
  geom_line(aes(y = rmse,
                group = noise_type,
                colour = noise_type), lwd =  1.3) +
  ylab("RMSE") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 0.7),
                  xlim = c(0, 150)) +
  scale_discrete_manual(values = colour_palette[c(3,5)],
                        aesthetics = c("colour", "fill"),
                        name = "Noise model") +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        strip.text = element_text(size = 24),
        plot.margin = margin(17, 5, 0, 5))

prow <- plot_grid(p1 + theme(legend.position="none"), 
          p2 + theme(legend.position="none"), 
          p3 + theme(legend.position="none"),
          nrow=1)

legend_b <- get_legend(p1 + theme(legend.position="bottom"))

p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .04))

p

ggsave(filename = file.path(PLOT_DIR, "Figure_basic_validation.png"), dpi = 320, height = 40, width = 50, units="cm")

