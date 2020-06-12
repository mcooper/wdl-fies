library(raster)
library(rgdal)
library(tidyverse)

setwd('data/covars_nowcast/')

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

r <- raster('data/covars_nowcast/rawdata/irrigation_aei.asc')

e <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, irrigation_aei)

res$irrigation_aei[is.na(res$irrigation_aei)] <- 0

write.csv(res, 'data/covars_nowcast/results/irrigation.csv', row.names=F)
