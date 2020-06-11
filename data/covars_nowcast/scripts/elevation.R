library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

ruggedness <- function(x){
  x <- as.vector(x)
  d <- x[c(1:4, 6:9)] - x[5]
  d <- sqrt(mean(d^2))
  return(d)
}

#FROM 
#https://www.eea.europa.eu/data-and-maps/data/world-digital-elevation-model-etopo5
p <- raster('~/wdl-fies/data/nowcast/rawdata/elevation/alwdgg.tif')
r <- focal(p, w=matrix(rep(1, 9), nrow=3), fun=ruggedness) 

ep <- raster::extract(p, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)
er <- raster::extract(r, sp, method='simple', fun=mean, na.rm=T,
              sp=TRUE, df=TRUE)

resp <- ep@data %>%
  select(GDLCODE=GDLcode, elevation=alwdgg)
resr <- er@data %>%
  select(GDLCODE=GDLcode, ruggedness=layer)

c <- merge(resp, resr)

write.csv(c, '~/wdl-fies/data/nowcast/results/elevation_ruggedness.csv', row.names=F)
