library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

nd <- raster('rawdata/nutrition_diversity_mfad.asc')

e <- raster::extract(nd, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, nutrition_diversity_mfad)

write.csv(res, 'results/nutritional_diversity_mfad.csv', row.names=F)
