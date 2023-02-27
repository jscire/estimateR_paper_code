here::i_am("scripts/comparison_dengue/dengue_preprocess.R")

library(fitdistrplus)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data from:
# Bastos, Leonardo S, Theodoros Economou, Marcelo F C Gomes, Daniel A M Villela,
# Flavio C Coelho, Oswaldo G Cruz, Oliver Stoner, Trevor Bailey, and Claudia T Codeço.
# “A Modelling Approach for Correcting Reporting Delays in Disease Surveillance Data.”
# Statistics in Medicine 38, no. 22 (2019): 4363–77. https://doi.org/10.1002/sim.8303.
# See https://github.com/lsbastos/Delay/tree/master/Code/Dengue
load(here::here("data","empirical_data","denguesinan.RData"))

## -------------------------------------------------------------------------------------------------
# Fit reporting delay distribution
# We use a log-normal distribution, which performed best
delay_samples <- d %>%
  filter(DT_NOTIFIC-DT_SIN_PRI<=365) %>% 
  filter(DT_DIGITA >= DT_SIN_PRI, DT_NOTIFIC >= DT_SIN_PRI) %>% 
  mutate(reporting_delay = DT_DIGITA - DT_SIN_PRI) %>% 
  filter(reporting_delay<=100) %>%  # cut off at 100 days delay
  mutate(reporting_delay=ifelse(reporting_delay==0,0.1,reporting_delay))

delay_dists <- lapply(list("2011"=2011,"2012"=2012,"2013"=2013,"2012-2013"=c(2012,2013)), function(year) {
  ds <- delay_samples %>% 
  filter(NU_ANO %in% year) %>% 
  pull(reporting_delay) %>%
    as.numeric()
  fitted <- fitdist(ds[sample.int(length(ds), 5000)], "lnorm") # need to subsample, too much data
  return(fitted$estimate)
})

d_delay_dists <- bind_rows(lapply(list("2011"=2011,"2012"=2012,"2013"=2013), function(x) {
  data.frame(reporting_delay = 1:200, p = dlnorm(1:200, meanlog = delay_dists[[as.character(x)]]["meanlog"], sdlog = delay_dists[[as.character(x)]]["sdlog"]))
}), .id = "NU_ANO")

saveRDS(delay_dists, here::here("data","empirical_data_results","comparison_dengue","delay_dists.rds"))


## -------------------------------------------------------------------------------------------------
# Preprocess incidence data by date of report
dengue_prep <- d %>%
  filter(DT_DIGITA >= DT_SIN_PRI, DT_NOTIFIC >= DT_SIN_PRI) %>% 
  count(date = DT_DIGITA, name = "confirm") %>% 
  complete(date = seq.Date(min(date),max(date),by="1 day"), fill = list(confirm=0))

saveRDS(dengue_prep, here::here("data","empirical_data","preprocessed.rds"))