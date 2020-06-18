library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

fs <- list.files('data/covars/rawdata/livestock', pattern='Aw.tif$', full.names = T, recursive=T)

s <- stack(fs)
a <- raster('data/covars/rawdata/livestock/8_Areakm.tif')

s <- s/a

names(s) <- gsub('.tif', '', basename(fs))

e <- raster::extract(s, sp, method='simple', df=TRUE, sp=TRUE, fun=mean, na.rm=T)

res <- e@data %>%
	select(GDLCODE=GDLcode, matches('X6')) %>%
  gather(animal, rate_per_km, -GDLCODE) %>%
  mutate(animal = case_when(grepl("Bf", animal) ~ "buffaloes",
                            grepl("Ch", animal) ~ "chickens",
                            grepl("Ct", animal) ~ "cattle",
                            grepl("Dk", animal) ~ "ducks",
                            grepl("Ho", animal) ~ "horses",
                            grepl("Pg", animal) ~ "pigs",
                            grepl("Sh", animal) ~ "sheep")) %>%
  spread(animal, rate_per_km)

res[is.na(res)] <- 0

write.csv(res, 'data/covars/results/livestock.csv', row.names=F)
