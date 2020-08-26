setwd('~/wdl-fies')

library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

ndvi <- raster('data/covars/rawdata/AVHRR/2016_vi_mn_75_100.tif')/10000
               
vcf <- stack('data/covars/rawdata/AVHRR/VCF_2016.tif')
forest <- vcf[[1]]
bare <- vcf[[3]]

s <- stack(ndvi, forest, bare)
names(s) <- c('avhrr_ndvi', 'avhrr_forest', 'avhrr_bare')

e <- raster::extract(s, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, avhrr_ndvi, avhrr_forest, avhrr_bare)

#Missing NDVI data for malaysian island, use country mean
res$avhrr_ndvi[res$GDLCODE == 'MYSr115'] <- mean(res$avhrr_ndvi[grepl('MYS', res$GDLCODE)], na.rm=T)

write.csv(res, 'data/covars/results/avhrr_vars.csv', row.names=F)
