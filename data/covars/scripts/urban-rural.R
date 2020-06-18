library(raster)
library(rgdal)
library(tidyverse)
library(imputeTS)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

#Historic Population
ref_past <- read.csv('data/covars/results/gdl_vars.csv') %>%
    dplyr::select(GDLCODE, YEAR, population)
ref_fut <- #Need future Population

#FROM https://www.worldclim.org/data/worldclim21.html
ur <- stack(list.files('data/covars/rawdata/urban-rural', full.names=T))

e <- raster::extract(ur, sp, method='simple', fun=sum, na.rm=T,
                                    sp=TRUE, df=TRUE)

res <- e@data %>%
  dplyr::select(GDLCODE=GDLcode, matches('ssp2')) %>%
  gather(Var, Pop, -GDLCODE) %>%
  mutate(YEAR=as.numeric(substr(Var, nchar(Var) - 3, nchar(Var))),
         UrbRur=ifelse(grepl("rur", Var), "Rural", "Urban"),
         Pop=Pop*1000) %>%
  dplyr::select(-Var) %>%
  spread(UrbRur, Pop) %>%
  group_by(GDLCODE) %>%
  complete(YEAR=2010:2030) %>%
  mutate(Rural=na_interpolation(x=Rural),
        Urban=na_interpolation(x=Urban),
        Total=Rural + Urban)

r <- merge(res, ref, all.x=T, all.y=F) %>%
  mutate(Frac = Urban/(Urban + Rural),
         urban=population*Frac,
         rural=population*(1 - Frac),
         total=urban + rural) %>%
  dplyr::select(GDLCODE, YEAR, rural, urban)

#For missing values in Vanuatu, use country means
r <- r %>%
  group_by(YEAR) %>%
  mutate(urban = ifelse(is.na(urban) & grepl('VUT', GDLCODE), 
                        mean(urban[grepl('VUT', GDLCODE)], na.rm=T),
                             urban),
         rural = ifelse(is.na(rural) & grepl('VUT', GDLCODE),
                        mean(rural[grepl('VUT', GDLCODE)], na.rm=T),
                        rural))

write.csv(r, 'data/covars/results/urban-rural.csv', row.names=F)
    
