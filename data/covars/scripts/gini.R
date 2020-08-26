setwd('~/wdl-fies')

library(tidyverse)
library(readxl)

#gini
gini <- read_xlsx("data/covars/rawdata/gini/Gini_projections_SSPs.xlsx",
                  sheet = "projected_ginis_full-set") %>%
  filter(scenario == "SSP2", year %in% 2011:2030) %>% 
  select(-scenario) %>%
  gather(variable, value, -year) %>% 
  mutate(gini = value/100, ISO3 = as.character(variable)) %>%
  select(ISO3, YEAR=year, gini)

write.csv(gini, 'data/covars/results/gini.csv', row.names=F)
