library(tidyverse)
library(countrycode)

options(stringsAsFactors=F)

iso <- read.csv('data/covars_nowcast/results/gdl_vars.csv') %>%
  group_by(ISO3, YEAR) %>%
  summarize(population=sum(population, na.rm=T)) %>%
  filter(YEAR >= 2010)

#From https://stats.oecd.org/viewhtml.aspx?datasetcode=CPA&lang=en
dat <- read.csv('data/covars_nowcast/rawdata/cpa.csv') %>%
  mutate(ISO3=countrycode(Recipient, 'country.name', 'iso3c'),
         ISO3=ifelse(Recipient=='Micronesia', 'FSM', ISO3),
         Value=Value*1000000) %>%
  filter(!is.na(ISO3),
         AMOUNTTYPE=='D',
         ISO3 %in% iso$ISO3) %>%
  select(YEAR=Year, CPA=Value, ISO3) %>%
  merge(iso, all.x=T, all.y=T) %>%
  mutate(CPA=ifelse(is.na(CPA), 0, CPA),
         cpa_percap=CPA/population) %>%
  filter(YEAR <= 2018) %>%
  select(ISO3, YEAR, cpa_percap)

write.csv(dat, 'data/covars_nowcast/results/cpa_percap.csv', row.names=F)
