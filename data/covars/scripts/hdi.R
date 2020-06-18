library(tidyverse)
library(sf)
library(readxl)
library(countrycode)
library(zoo)

options(stringsAsFactors=F)

sf <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

####################
#### Past data
#####################
past <- read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLcode) %>%
  select(GDLCODE, YEAR=year, admin_hdi=shdi, pop) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3)) %>%
  group_by(ISO3, YEAR) %>%
  mutate(country_hdi = weighted.mean(admin_hdi, pop)) %>%
  select(-pop)

####################
#### Future (SSP2) data
####################

#hdi
hdi <- read_xlsx("data/covars/rawdata/hdi/HDI-SSPs.xlsx", sheet = "Tabelle1") %>%
  mutate(ISO3 = countrycode(gsub(' - \\d+','', obs), 
                            origin = "country.name", 
                            destination = "iso3c"),
         YEAR = as.numeric(paste0("20", gsub('\\D','', obs)))) %>%
  select(ISO3, YEAR, country_hdi=HDI_SSP2) %>%
  filter(YEAR <= 2030, YEAR >= 2020) %>%
  merge(expand.grid(list(GDLCODE=unique(past$GDLCODE),
                         YEAR=2020:2030)) %>%
          mutate(ISO3 = substr(GDLCODE, 1, 3)),
        all.y=T)

#######################################
# Combine and disaggregate future Admin 1 by Past
#######################################

comb <- bind_rows(hdi, past) %>% 
  group_by(ISO3, GDLCODE) %>%
  complete(YEAR=1990:2030) %>%
  arrange(YEAR) %>%
  mutate(country_hdi = na.approx(country_hdi, na.rm=F),
         admin_hdi = na.approx(admin_hdi, na.rm=F))
  
write.csv(comb, 'data/covars/results/hdi.csv', row.names=F)


