library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

fs <- list.files('rawdata/malaria', pattern='^2019_Global_P._Incidence_20...tif$', full.names = T, recursive=T)

s <- stack(fs)

e <- raster::extract(s, sp)

e2 <- aggregate(e, list(newdat$GDLCode), FUN=mean, na.rm=T) %>%
  gather(timetype, incidence, -Group.1) %>%
  mutate(year=as.numeric(substr(timetype, nchar(timetype) - 3, nchar(timetype))),
         type=ifelse(grepl('Pf', timetype), 'falciparum', 'vivax')) %>%
  select(-timetype) %>%
  rename(GDLCode=Group.1) %>%
  spread(type, incidence)

write.csv(e2, 'whc/GDL_malaria.csv', row.names=F)
