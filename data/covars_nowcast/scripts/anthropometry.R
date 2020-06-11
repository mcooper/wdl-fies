library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

stunting <- stack(list.files('rawdata/stunting', full.names=T))
wasting <- stack(list.files('rawdata/wasting', full.names=T))
      
es <- raster::extract(stunting, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

ew <- raster::extract(wasting, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)


em <- merge(es@data, ew@data) %>%
  dplyr::select(GDLCODE=GDLcode, matches("IHME")) %>%
  gather(var, val, -GDLCODE) %>%
  mutate(YEAR = as.numeric(str_match(var, "MEAN_(\\d+)")[ , 2]),
         var = ifelse(grepl('STUNT', var), 'stunting', 'wasting')) %>%
  spread(var, val)

write.csv(em, 'results/anthro_vars.csv', row.names=F)
