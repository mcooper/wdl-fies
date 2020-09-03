setwd('~/wdl-fies')

library(tidyverse)
library(zoo)
library(sf)

sp <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

pop <- read.csv('data/covars/rawdata/ssp/SspDb_country_data_2013-06-12.csv')

u5 <- pop %>%
  filter(grepl('Aged0-4$', VARIABLE), 
         MODEL=='IIASA-WiC POP', 
         SCENARIO=='SSP2_v9_130115') %>%
  dplyr::select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  group_by(ISO3) %>%
  summarize_all(sum) %>%
  gather(YEAR, u5pop, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5)))

total <- pop %>%
  filter(VARIABLE == 'Population',
         MODEL=='IIASA-WiC POP', 
         SCENARIO=='SSP2_v9_130115') %>%
  dplyr::select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  group_by(ISO3) %>%
  summarize_all(sum) %>%
  gather(YEAR, total, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5)))

comb <- merge(u5, total) %>%
  mutate(u5_frac = u5pop/total)

#For South Sudan use values from CAR
comb <- bind_rows(comb,
                  comb %>%
                    filter(ISO3 == 'CAF') %>%
                    mutate(ISO3 = 'SSD')) %>%
  select(-u5pop, -total)

write.csv(comb, 'data/u5_population.csv', row.names=F)
