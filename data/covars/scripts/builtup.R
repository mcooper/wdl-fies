library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

r <- raster('data/covars/rawdata/builtup.tif')

r <- projectRaster(r, crs=sp@proj4string)

r <- aggregate(r, fact=10, fun=mean, na.rm=T)

e <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, esa_builtup=builtup)

write.csv(res, 'data/covars/results/builtup.csv', row.names=F)
