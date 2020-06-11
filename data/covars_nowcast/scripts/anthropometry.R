library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

stunting <- stack(list.files('data/covars_nowcast/rawdata/stunting', full.names=T))
wasting <- stack(list.files('data/covars_nowcast/rawdata/wasting', full.names=T))
      
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

#Set developing countries to 0
em$stunting[is.na(em$stunting)] <- 0
em$wasting[is.na(em$wasting)] <- 0

#Add 2018 using 2017 values
e4 <- bind_rows(em,
                em %>%
                  filter(YEAR==2017) %>%
                  mutate(YEAR=2018))

write.csv(e4, 'data/covars_nowcast/results/anthro_vars.csv', row.names=F)
