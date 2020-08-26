setwd('~/wdl-fies')

library(raster)
library(rgdal)
library(tidyverse)

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
