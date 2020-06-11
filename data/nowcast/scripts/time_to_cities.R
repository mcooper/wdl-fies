library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

r <- raster('rawdata/accessibility_to_cities.tif')

r <- aggregate(r, fact=10, fun=mean, na.rm=T)

e <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, market_dist=layer)

write.csv(res, 'results/market_dist.csv', row.names=F)
