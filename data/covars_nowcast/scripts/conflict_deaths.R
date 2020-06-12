library(tidyverse)
library(raster)
library(lubridate)
library(rgdal)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

d <- read.csv('data/covars_nowcast/rawdata/conflict_deaths/ged191.csv') %>%
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

all <- merge(expand.grid(list(GDLCODE=unique(sp$GDLcode),
                         YEAR=2014:2018)),
             all, all.x=T, all.y=T)

all$conflict_deaths[is.na(all$conflict_deaths)] <- 0

ref <- read.csv('data/covars_nowcast/results/gdl_vars.csv') %>%
  dplyr::select(GDLCODE, YEAR, population)

all <- merge(all, ref, all.x=T, all.y=F)

all <- all %>%
  mutate(conflict_deaths_percap=conflict_deaths/population) %>%
  dplyr::select(GDLCODE, YEAR, conflict_deaths_percap) %>%
  na.omit

write.csv(all, 'data/covars_nowcast/results/conflict_deaths.csv', row.names=F)
