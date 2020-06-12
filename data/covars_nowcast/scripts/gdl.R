library(tidyverse)
library(sf)

options(stringsAsFactors=F)

sf <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

#https://globaldatalab.org/shdi/download_files/
#https://globaldatalab.org/assets/2020/03/SHDI%20Complete%204.0%20%281%29.csv
dat <- read.csv('data/covars_nowcast/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLcode) %>%
  mutate(gni_percap_2011=gnic*1000,
         population=pop*1000000) %>%
  select(GDLCODE, 
         YEAR=year, life_expectancy=lifexp, gni_percap_2011, years_school_expected=esch,
         years_school_mean=msch, population, hdi=shdi)

all <- expand.grid(list(GDLCODE=unique(dat$GDLCODE), YEAR=1990:2018)) %>%
  merge(dat, all.x=T, all.y=F) %>%
  gather(var, value, -GDLCODE, -YEAR) %>%
  group_by(GDLCODE, var) %>%
  arrange(YEAR) %>%
  fill(value) %>%
  arrange(desc(YEAR)) %>%
  fill(value) %>%
  spread(var, value) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3))

write.csv(all, 'data/covars_nowcast/results/gdl_vars.csv', row.names=F)
