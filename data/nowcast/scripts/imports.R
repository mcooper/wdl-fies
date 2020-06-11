library(tidyverse)
library(countrycode)

ref <- read.csv('data/nowcast/results/gdl_vars.csv') %>%
  group_by(iso3c, YEAR) %>%
  summarize(population=sum(population, na.rm=T))

dat <- read.csv('data/nowcast/rawdata/imports/API_NE.IMP.GNFS.CD_DS2_en_csv_v2_1120983.csv', skip=4) %>%
  mutate(iso3c = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, imports, -iso3c) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(iso3c),
         YEAR >= 1990) %>%
  arrange(desc(YEAR)) %>%
  group_by(iso3c) %>%
  fill(imports) %>%
  arrange(YEAR) %>%
  group_by(iso3c) %>%
  fill(imports)

#Need to impute: AND, LIE, STP, TTO, YEM
#Andorra to Luxembourg
dat$imports[dat$iso3c=='AND'] <- dat$imports[dat$iso3c=='LUX']
#Liechtenstein to Luxembourg
dat$imports[dat$iso3c=='LIE'] <- dat$imports[dat$iso3c=='LUX']
#Sao Tme and Principe to Cape Verde
dat$imports[dat$iso3c=='STP'] <- dat$imports[dat$iso3c=='CPV']
#Trinidad & Tobago to Guyana
dat$imports[dat$iso3c=='TTO'] <- dat$imports[dat$iso3c=='GUY']
#Yemen to Somalia
dat$imports[dat$iso3c=='YEM'] <- dat$imports[dat$iso3c=='SOM']

#Covert to per capita
dat <- dat %>%
  merge(ref, all.x=F, all.y=F) %>%
  mutate(imports_percap=imports/population) %>%
  select(-imports, -population)

write.csv(dat, 'data/nowcast/results/imports_percap.csv')
