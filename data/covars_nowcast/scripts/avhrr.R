library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

ndvi <- raster('rawdata/AVHRR/2016_vi_mn_75_100.tif')/10000
               
vcf <- stack('rawdata/AVHRR/VCF_2016.tif')
forest <- vcf[[1]]
bare <- vcf[[3]]

s <- stack(ndvi, forest, bare)
names(s) <- c('ndvi', 'forest', 'bare')

e <- raster::extract(s, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, ndvi, forest, bare)

write.csv(res, 'results/avhrr_vars.csv', row.names=F)
