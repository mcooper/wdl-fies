library(tidyverse)
library(countrycode)

dat <- read.csv('data/covars/rawdata/ag_pct_gdp/API_NV.AGR.TOTL.ZS_DS2_en_csv_v2_1122041.csv', skip=4) %>%
  mutate(ISO3 = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, ag_pct_gdp, -ISO3) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(ISO3),
         YEAR >= 1990) %>%
  arrange(desc(YEAR)) %>%
  group_by(ISO3) %>%
  fill(ag_pct_gdp) %>%
  arrange(YEAR) %>%
  group_by(ISO3) %>%
  fill(ag_pct_gdp)

#Liechtenstein <- Luxembourg
dat$ag_pct_gdp[dat$ISO3=="LIE"] <- dat$ag_pct_gdp[dat$ISO3=='LUX']

write.csv(dat, 'data/covars/results/ag_pct_gdp.csv', row.names=F)
