setwd('~/wdl-fies')

library(tidyverse)
library(countrycode)

ref <- read.csv('data/covars/results/gdl_vars.csv') %>%
  group_by(ISO3, YEAR) %>%
  summarize(population=sum(population, na.rm=T))

dat <- read.csv('data/covars/rawdata/imports/API_NE.IMP.GNFS.CD_DS2_en_csv_v2_1120983.csv', skip=4) %>%
  mutate(ISO3 = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, imports, -ISO3) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(ISO3),
         YEAR >= 1990) %>%
  arrange(desc(YEAR)) %>%
  group_by(ISO3) %>%
  fill(imports) %>%
  arrange(YEAR) %>%
  group_by(ISO3) %>%
  fill(imports)

#Need to impute: AND, LIE, STP, TTO, YEM
#Andorra to Luxembourg
dat$imports[dat$ISO3=='AND'] <- dat$imports[dat$ISO3=='LUX']
#Liechtenstein to Luxembourg
dat$imports[dat$ISO3=='LIE'] <- dat$imports[dat$ISO3=='LUX']
#Sao Tme and Principe to Cape Verde
dat$imports[dat$ISO3=='STP'] <- dat$imports[dat$ISO3=='CPV']
#Trinidad & Tobago to Guyana
dat$imports[dat$ISO3=='TTO'] <- dat$imports[dat$ISO3=='GUY']
#Yemen to Somalia
dat$imports[dat$ISO3=='YEM'] <- dat$imports[dat$ISO3=='SOM']

#Covert to per capita
dat <- dat %>%
  merge(ref, all.x=F, all.y=F) %>%
  mutate(imports_percap=imports/population) %>%
  select(-imports, -population)

write.csv(dat, 'data/covars/results/imports_percap.csv', row.names=F)
