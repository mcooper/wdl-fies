library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

r <- raster('rawdata/BuiltUp2014.tif')

e <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, builtup=layer)

write.csv(res, 'results/builtup.csv', row.names=F)
