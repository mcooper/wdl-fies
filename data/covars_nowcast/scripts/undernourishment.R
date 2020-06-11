library(tidyverse)
library(countrycode)

dat <- read.csv('~/wdl-fies/data/nowcast/rawdata/ag_perc_gdp/API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_1122041.csv', skip=4) %>%
  mutate(iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, undernourishment, -iso3c) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(iso3c),
         YEAR >= 1990) %>%
  arrange(desc(YEAR)) %>%
  group_by(iso3c) %>%
  fill(undernourishment) %>%
  arrange(YEAR) %>%
  group_by(iso3c) %>%
  fill(undernourishment)

write.csv(dat, '~/wdl-fies/data/nowcast/results/undernourishment.csv')
