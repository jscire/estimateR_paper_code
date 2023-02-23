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

all_figure_data <- summarized_results
all_figure_data$simulation_type <-  recode_factor(all_figure_data$simulation_type, 
                                                      `Case confirmations` = "Case confirmations",
                                                      `Combined - Low prob.` = "Combined - Low prob.",
                                                     `Combined - High prob.` = "Combined - High prob.",
                                                      `Symptom onsets` = "Symptom onsets")

main_figure_data <- filter(all_figure_data, 
                           noise_type == "autocorrelated",
                           bootstrapping_type == "bootstrapped")

main_figure_data <- main_figure_data %>% mutate(Rt_type = recode(Rt_type,
                                                                 `Abrupt decrease` = "(1) Abrupt\n    decrease",
                                                                 `Abrupt increase` = "(2) Abrupt\n    increase",
                                                                 `Constant` = "(3) Constant",
                                                                 `Linear decrease` = "(4) Linear\n    decrease",
                                                                 `Linear increase` = "(5) Linear\n    increase"))


colour_palette <- viridis(7)

theme_set(theme_bw() +
            theme(
              strip.text = element_text(size = 24),
              axis.title = element_text(size = 28),
              axis.text = element_text(size = 20),
              panel.spacing.y = unit(0.9, "lines"),
              legend.title = element_text(size = 26),
              legend.text = element_text(size = 24)
            ))

p1 <- ggplot(main_figure_data, aes(x = idx)) +
  # facet_grid(Rt_type ~ simulation_type, labeller = label_wrap_gen(width = 12)) + 
  facet_grid(Rt_type ~ .) + 
  geom_line(aes(y = ref_Rt), lwd =  1, colour = "black") +
  geom_line(aes(y = median_Re_estimate,
                group = simulation_type, 
                colour = simulation_type), 
            lwd =  1.2) +
  geom_ribbon(aes(ymax = median_CI_up_Re_estimate, 
                  ymin = median_CI_down_Re_estimate,
                  group = simulation_type,
                  fill = simulation_type),
              alpha = 0.10) +
  ggtitle("(A) Reproductive number") +
  xlab("Time") +
  coord_cartesian(ylim = c(0, 3),
                  xlim = c(0, 150)) +
  scale_colour_viridis_d(name = "Observations:", option = "B", begin = 0.15, end= 0.95) +
  scale_fill_viridis_d(name = "Observations:", option = "B",  begin = 0.15, end= 0.95) +
  theme(strip.text.y = element_blank(), strip.background = element_rect(colour="black",
                                  fill="white"),
        plot.margin = margin(5, 5, 0, 15),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24))



p2 <- ggplot(main_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .) + 
  geom_line(aes(y = coverage,
                group = simulation_type, 
                colour = simulation_type), 
            lwd =  1.2) +
  ggtitle("(B) Coverage") +
  xlab("Time") +
  scale_colour_viridis_d(option = "B", begin = 0.15, end= 0.95) +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 150)) +
  theme(strip.text.y = element_blank(),
        strip.background.x = element_rect(colour="black",
                                          fill="white"),
        plot.margin = margin(5, 10, 0, 15),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24))

p3 <- ggplot(main_figure_data, aes(x = idx)) +
  facet_grid(Rt_type ~ .) + 
  geom_line(aes(y = rmse,
                group = simulation_type, 
                colour = simulation_type), 
            lwd =  1.2) +
  ggtitle("(C) RMSE") +
  xlab("Time") +
  scale_colour_viridis_d(option = "B", begin = 0.15, end= 0.95) +
  coord_cartesian(ylim = c(0, 0.7),
                  xlim = c(0, 150)) +
  theme(strip.background = element_rect(colour="black",
                                        fill="white"),
        plot.margin = margin(5, 5, 0, 5),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 24))

prow <- plot_grid(p1 + theme(legend.position="none"), 
                  p2 + theme(legend.position="none"), 
                  p3 + theme(legend.position="none"),
                  nrow=1)

legend_b <- get_legend(p1 + theme(legend.position="bottom"))

p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .04))

p

ggsave(filename = file.path(PLOT_DIR, "Figure_combining_main.png"), dpi = 320, width = 50, height = 40, units="cm")

