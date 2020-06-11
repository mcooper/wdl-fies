library(raster)
library(rgdal)
library(tidyverse)
library(imputeTS)

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

ref <- read.csv('~/wdl-fies/data/nowcast/results/gdl_vars.csv') %>%
	select(GDLCODE, YEAR, population)

#FROM https://www.worldclim.org/data/worldclim21.html
ur <- stack(list.files('~/wdl-fies/data/nowcast/rawdata/urban-rural', full.names=T))

e <- raster::extract(ur, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, ssp2rur2010, ssp2rur2020, ssp2urb2010, ssp2urb2020) %>%
  gather(Var, Pop, -GDLCODE) %>%
  mutate(YEAR=as.numeric(substr(Var, nchar(Var) - 3, nchar(Var))),
         UrbRur=ifelse(grepl("rur", Var), "Rural", "Urban"),
				 Pop=Pop*1000) %>%
  dplyr::select(-Var) %>%
  spread(UrbRur, Pop) %>%
  merge(expand.grid(list(YEAR=2010:2020, GDLCODE=unique(sp@data$GDLcode))), all.y=T) %>%
  group_by(GDLCODE) %>%
  mutate(Rural=na_interpolation(x=Rural),
         Urban=na_interpolation(x=Urban),
				 Total=Rural + Urban)

r <- merge(res, ref, all.x=T, all.y=F) %>%
	mutate(Frac = Urban/(Urban + Rural),
				 Urban=population*Frac,
				 Rural=population*(1 - Frac),
				 Total=Urban + Rural) %>%
	select(GDLCODE, YEAR, Rural, Urban)

write.csv(r, '~/wdl-fies/data/nowcast/results/urban-rural.csv', row.names=F)
