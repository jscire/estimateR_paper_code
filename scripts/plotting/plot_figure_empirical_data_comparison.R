library(tidyverse)
library(here)
library(viridis)
library(ggplot2)
library(cowplot)

estimateR_results_dir <- here::here("data", "empirical_data_results")
PLOT_DIR <- here::here("plots")

dataServer <- read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/CHE-confCasesSWestimates.csv") %>%
  filter(!str_detect(region, "seR")) %>%
  bind_rows(read_csv("https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/LIE-confCasesSWestimates.csv")) %>%
  mutate(
    version = "server",
    region = recode(region,
                    "LIE" = "FL",
                    "CHE" = "CH",
                    "grR Central Switzerland" = "gr-CCH",
                    "grR Eastern Switzerland" = "gr-ECH",
                    "grR Espace Mittelland" = "gr-EM",
                    "grR Lake Geneva Region" = "gr-LGR",
                    "grR Northwestern Switzerland" = "gr-NCH",
                    "grR Ticino" = "gr-TI",
                    "grR Zurich" = "gr-ZH"
    )
  ) %>%
  arrange(region, date) %>%
  select(
    date, region,
    Re_estimate = median_R_mean,
    CI_down_Re_estimate = median_R_lowHPD,
    CI_up_Re_estimate = median_R_highHPD,
    version)

dataNew <- read_csv(file.path(estimateR_results_dir, "2021-10-19_swiss-estimates.csv")) %>%
  arrange(region, date) %>%
  mutate(version = "new")

setequal(unique(dataServer$region), unique(dataNew$region))

allData <- bind_rows(dataServer, dataNew)

allData %>%
  filter(!is.na(Re_estimate)) %>%
  group_by(version, region) %>%
  summarise(
    maxDate = max(date),
    minDate = min(date),
    .groups = "drop") %>%
  pivot_wider(
    names_from = version,
    names_sep = ".",
    values_from = c(maxDate, minDate))

plotData <- allData %>% 
  rename(Software=version)


# Add epinow2

epinow2_r <- readRDS(here::here("data","empirical_data_results","epinow2_r.rds"))

plotData <- bind_rows(plotData,
          epinow2_r %>%
            filter(type == "estimate") %>%
            filter(date < estimation_date - 21, date > first_date + 21) %>% 
            group_by(country, date) %>% 
            summarize(across(where(is.numeric), ~median(., na.rm = T)), .groups = "drop") %>% 
            filter(country == "Switzerland", date <= "2021-10-09") %>% 
            select(date, Re_estimate = median, CI_down_Re_estimate = lower_90, CI_up_Re_estimate = upper_90) %>% 
            mutate(region = "CH", Software = "EpiNow2", .after = date)
)


# Factor
plotData$Software <- recode_factor(plotData$Software,
                                   new="estimateR",
                                   server="Huisman et al. pipeline",
                                   EpiNow2="EpiNow2")



colour_palette <- viridis(7)
p1 <- plotData %>%
  filter(region == "CH") %>%
  ggplot(aes(x = date, y = Re_estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_line(aes(colour = Software), lwd =  1.1) +
  geom_ribbon(aes(x = date, ymax = CI_up_Re_estimate, ymin = CI_down_Re_estimate, fill = Software),
              alpha = 0.25, colour = NA) +
  geom_text(x=as.Date("2020-07-14"), y=2.2, label="Switzerland",size = 6, hjust = 0) +
  scale_colour_manual(values = colour_palette[c(1,5,3)], aesthetics = c("colour", "fill")) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b-%d\n%Y",
               limits = c(as.Date("2020-07-01"), as.Date("2021-09-14")),
               expand = c(0,0)) +
  coord_cartesian(ylim = c(0.4, 2.4)) +
  scale_y_continuous(expand = c(0,NA)) +
  xlab("") +
  theme_bw() +
  ylab("Reproductive number") +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        legend.position = c(.7, .85),
        legend.background = element_blank())
        # plot.margin = margin(15, 15, 15, 15))
p1

# Make subplots for other countries
EMPIRICAL_DATA_DIR <- here::here("data", "empirical_data")
RESULTS_DIR <- here::here("data", "empirical_data_results")

make_subplot <- function(plot_number) {
  # AUS BEL CHL GBR IND JPN USA ZAF
  country_numbers <- c(8, 10, 25, 50, 61, 71, 153, 160)
  country_names <- c("Australia",
                     "Belgium",
                     "Chile",
                     "United Kingdom",
                     "Indonesia",
                     "Japan",
                     "United States of America",
                     "South Africa")
  results_files <- list.files(path = RESULTS_DIR, pattern = "*_compared_estimates.csv", full.names = F)
  result_file <- results_files[country_numbers[plot_number]]
  country_code <- unlist(strsplit(result_file, split = "_"))[1]
  print(country_code)
  result_data <- read_csv(file = file.path(RESULTS_DIR, result_file),
                          col_types = list(
                            date = col_date(format = ""),
                            Re_estimate = col_double(),
                            CI_up_Re_estimate = col_double(),
                            CI_down_Re_estimate = col_double(),
                            implementation = col_character()
                          ))
  
  plotData <- result_data %>% 
    rename(Software=implementation)
  
  plotData <- bind_rows(plotData,
                        epinow2_r %>%
                          filter(type == "estimate") %>%
                          filter(date < estimation_date - 21, date > first_date + 21) %>% 
                          group_by(country, date) %>% 
                          summarize(across(where(is.numeric), ~median(., na.rm = T)), .groups = "drop") %>% 
                          mutate(country = ifelse(country=="United States","United States of America",country)) %>% 
                          filter(country == country_names[plot_number], date <= "2021-10-09") %>% 
                          select(date, Re_estimate = median, CI_down_Re_estimate = lower_90, CI_up_Re_estimate = upper_90) %>% 
                          mutate(region = "CH", Software = "EpiNow2", .after = date)
  )
  
  plotData$Software <- recode_factor(plotData$Software,
                                     estimateR="estimateR",
                                     pipeline="Huisman et al. pipeline",
                                     EpiNow2="EpiNow2")
  
  colour_palette <- viridis(7)
  y_axis_label <- if_else(plot_number %in% c(3,6), "Reproductive number", "")
  p1 <- ggplot(data = plotData,
               mapping = aes(x = date)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_line(aes(y = Re_estimate,
                  colour = Software,
                  group = Software),
              lwd = 1.1) +
    geom_ribbon(aes(ymax = CI_up_Re_estimate,
                    ymin = CI_down_Re_estimate,
                    group = Software,
                    fill = Software), alpha = 0.25, colour = NA) +
    geom_text(x=as.Date("2020-07-14"), y=2.2, label=country_names[plot_number], size = 6, hjust = 0) +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%b-%d\n%Y",
                 limits = c(as.Date("2020-07-01"), as.Date("2021-09-14")),
                 expand = c(0,0)) +
    scale_colour_manual(values = colour_palette[c(1,5,3)], aesthetics = c("colour", "fill")) +
    ylab(y_axis_label) +
    coord_cartesian(ylim = c(0.4, 2.4)) +
    scale_y_continuous(expand = c(0,NA)) +
    xlab("") +
    theme_bw() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 13),
          legend.text = element_text(size = 13),
          legend.title = element_blank(),
          legend.position = "none")
          # plot.margin = margin(15, 15, 15, 15))
  
  return(p1)
}




subplots <- lapply(1:8, make_subplot)

all_subplots <- c(list(p1), subplots)
plot_grid(plotlist = all_subplots,
          ncol=3,
          nrow=3,
          labels = LETTERS[1:9],
          label_size = 20)


ggsave(filename = file.path(PLOT_DIR, "Figure_empirical_comparison.png"), width = 40, height = 30, units = "cm", dpi = 320)

