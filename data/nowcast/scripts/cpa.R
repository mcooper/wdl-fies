library(tidyverse)
library(countrycode)

options(stringsAsFactors=F)

iso <- read.csv('~/wdl-fies/data/nowcast/results/gdl_vars.csv') %>%
  group_by(iso3c, YEAR) %>%
  summarize(population=sum(population, na.rm=T)) %>%
  filter(YEAR >= 2010)

#From https://stats.oecd.org/viewhtml.aspx?datasetcode=CPA&lang=en
dat <- read.csv('~/wdl-fies/data/nowcast/rawdata/cpa.csv') %>%
  mutate(iso3c=countrycode(Recipient, 'country.name', 'iso3c'),
         iso3c=ifelse(Recipient=='Micronesia', 'FSM', iso3c),
         Value=Value*1000000) %>%
  filter(!is.na(iso3c),
         AMOUNTTYPE=='D',
         iso3c %in% iso$iso3c) %>%
  select(YEAR=Year, CPA=Value, iso3c) %>%
  merge(iso, all.x=T, all.y=T) %>%
  mutate(CPA=ifelse(is.na(CPA), 0, CPA),
         cpa_percap=CPA/population) %>%
  filter(YEAR <= 2018) %>%
  select(iso3c, YEAR, cpa_percap)

write.csv(dat, '~/wdl-fies/data/nowcast/results/cpa_percap.csv', row.names=F)
