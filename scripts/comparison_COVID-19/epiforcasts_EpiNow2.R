library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

here::i_am("scripts/comparison_with_other_packages/epiNow2_Covid_epiforcasts.R")

# Scrape national R estimates from epiforcasts
source("https://raw.githubusercontent.com/brshallo/save-versions-from-git/main/scripts/save_file_versions_from_git.R")
save_file_versions_from_github(
  file_url = "https://github.com/epiforecasts/covid-rt-estimates/blob/master/national/cases/summary/rt.csv",
  delete_clone = TRUE)

countries <- c("Switzerland", "Australia", "Belgium", "Chile", "United Kingdom", "Indonesia", "Japan", "United States", "South Africa")

# Combine estimates from scraped files
filelist <- as.list(list.files(here::here("data","empirical_data_results","covid-rt-estimates_rt"), full.names = T))
names(filelist) <- stringr::str_extract(list.files(here::here("data","empirical_data_results","covid-rt-estimates_rt")), "\\d{4}-\\d{2}-\\d{2}")
epinow2_r <- bind_rows(lapply(filelist, function(x) {
  df <- read_csv(x)
  if(nrow(df)>0){
    if ("country" %in% names(df)) {
      df %>% filter(country %in% countries) %>% return()
    } else {
      df %>% filter(region %in% countries) %>% return()
    } 
  }
  }), .id = "estimation_date")

epinow2_r <- epinow2_r %>%
  mutate(country=ifelse(is.na(country),region,country)) %>%
  select(-region) %>%
  mutate(estimation_date = as.Date(estimation_date)) %>% 
  group_by(country, estimation_date) %>% 
  mutate(first_date = min(date))

# save results
saveRDS(epinow2_r, here::here("data","empirical_data_results","epinow2_r.rds"))
