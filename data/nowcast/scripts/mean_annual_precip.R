library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

#FROM https://www.worldclim.org/data/worldclim21.html
p <- stack(list.files('~/wdl-fies/data/nowcast/rawdata/precipitation', 
                      pattern='tif$',
                      full.names=T))

p <- calc(p, mean, na.rm=T)

e <- raster::extract(p, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, mean_annual_precip=layer)

#One NA In Malaysia, use country average
res$mean_annual_precip[res$GDLCODE=='MYSr115'] <- mean(res$mean_annual_precip[grepl('MYS', res$GDLCODE)], na.rm=T)

write.csv(res, '~/wdl-fies/data/nowcast/results/mean_annual_precip.csv', row.names=F)
