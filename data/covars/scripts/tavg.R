setwd('~/wdl-fies')

library(raster)
library(rgdal)
library(tidyverse)

mod1_2010s <- stack('data/covars/rawdata/tavg/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20110101-20201231.nc4')
indices <- as.numeric(format(as.Date(names(mod1_2010s), format = "X%Y.%m.%d"), format = "%Y"))
mod1_2010s_year <- stackApply(mod1_2010s, indices, fun=mean)

mod1_2020s <- stack('data/covars/rawdata/tavg/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
indices <- as.numeric(format(as.Date(names(mod1_2020s), format = "X%Y.%m.%d"), format = "%Y"))
mod1_2020s_year <- stackApply(mod1_2020s, indices, fun=mean)

mod2_2010s <- stack('data/covars/rawdata/tavg/tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20110101-20201231.nc4')
indices <- as.numeric(format(as.Date(names(mod2_2010s), format = "X%Y.%m.%d"), format = "%Y"))
mod2_2010s_year <- stackApply(mod2_2010s, indices, fun=mean)

mod2_2020s <- stack('data/covars/rawdata/tavg/tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
indices <- as.numeric(format(as.Date(names(mod2_2020s), format = "X%Y.%m.%d"), format = "%Y"))
mod2_2020s_year <- stackApply(mod2_2020s, indices, fun=mean)



sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

#FROM https://www.worldclim.org/data/worldclim21.html
t <- stack(list.files('data/covars/rawdata/tavg', 
                      pattern='tif$',
                      full.names=T))

t <- calc(t, mean, na.rm=T)

e <- raster::extract(t, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, tavg=layer)

#One NA In Malaysia, use country average
res$tavg[res$GDLCODE=='MYSr115'] <- mean(res$tavg[grepl('MYS', res$GDLCODE)], na.rm=T)

write.csv(res, 'data/covars/results/tavg.csv', row.names=F)
