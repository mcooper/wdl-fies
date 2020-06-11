library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

fs <- list.files('rawdata/livestock', pattern='Aw.tif$', full.names = T, recursive=T)

s <- stack(fs)
a <- raster('rawdata/livestock/8_Areakm.tif')

s <- s/a

names(s) <- gsub('.tif', '', basename(fs))

e <- raster::extract(s, sp, method='simple', df=TRUE, sp=TRUE, fun=mean, na.rm=T)

res <- e@data %>%
	select(GDLCODE=GDLcode, matches('X6')) %>%
  gather(animal, rate_per_km, -GDLCODE) %>%
  mutate(animal = case_when(grepl("Bf", animal) ~ "Buffaloes",
                            grepl("Ch", animal) ~ "Chickens",
                            grepl("Ct", animal) ~ "Cattle",
                            grepl("Dk", animal) ~ "Ducks",
                            grepl("Ho", animal) ~ "Horses",
                            grepl("Pg", animal) ~ "Pigs",
                            grepl("Sh", animal) ~ "Sheep")) %>%
  spread(animal, rate_per_km)

write.csv(res, 'results/livestock.csv', row.names=F)
