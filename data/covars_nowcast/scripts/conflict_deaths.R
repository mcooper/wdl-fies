library(tidyverse)
library(raster)
library(lubridate)
library(rgdal)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

d <- read.csv('rawdata/conflict_deaths/ged191.csv') %>%
  dplyr::select(date_start, date_end, best, latitude, longitude) 

dsp <- SpatialPointsDataFrame(d, coords=d[ , c('longitude', 'latitude')], 
                              proj4string=sp@proj4string)

all <- data.frame()
for (y in 2014:2018){
  sel <- dsp[year(ymd(dsp$date_end)) == y, ]

  o <- over(sel, sp) %>%
    mutate(deaths=sel$best,
           YEAR=y) %>%
    group_by(GDLcode, YEAR) %>%
    summarize(conflict_deaths=sum(deaths, na.rm=T))

  all <- bind_rows(all, o)
}

all <- all %>%
  rename(GDLCODE=GDLcode)

ref <- read.csv('results/gdl_vars.csv') %>%
  dplyr::select(GDLCODE, YEAR, population)

all <- merge(all, ref, all.x=T, all.y=F)

all <- all %>%
  mutate(conflict_deaths_percap=conflict_deaths/population) %>%
  dplyr::select(GDLCODE, YEAR, conflict_deaths_percap) %>%
  na.omit

write.csv(all, 'results/conflict_deaths.csv', row.names=F)
