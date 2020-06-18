library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

r <- raster('data/covars/rawdata/accessibility_to_cities.tif')

r <- aggregate(r, fact=10, fun=mean, na.rm=T)

e <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, accessibility_to_cities)

res <- res %>%
  filter(!is.na(res$GDLCODE))

write.csv(res, 'data/covars/results/accessibility_to_cities.csv', row.names=F)
