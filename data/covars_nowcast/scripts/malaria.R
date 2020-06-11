library(raster)
library(rgdal)
library(tidyverse)

setwd('~/wdl-fies/data/nowcast/')

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

fs <- list.files('rawdata/malaria', pattern='^2019_Global_P._Incidence_20...tif$', full.names = T, recursive=T)

s <- stack(fs)

e <- raster::extract(s, sp)

getColSumsOrNull <- function(x){
	if (is.null(dim(x))){
		return(data.frame(X2019_Global_Pv_Incidence_2012=NA))
	} else{
		return(colSums(x, na.rm=T))
	}
}

e2 <- lapply(e, getColSumsOrNull) %>%
	bind_rows

e3 <- e2 %>%
	mutate(GDLCODE=sp$GDLcode) %>%
  gather(timetype, incidence, -GDLCODE) %>%
  mutate(YEAR=as.numeric(substr(timetype, nchar(timetype) - 3, nchar(timetype))),
         type=ifelse(grepl('Pf', timetype), 'falciparum', 'vivax')) %>%
  select(-timetype) %>%
  spread(type, incidence)

#Missing data at northern latitudes
e3$falciparum[is.na(e3$falciparum)] <- 0
e3$vivax[is.na(e3$vivax)] <- 0

write.csv(e3, 'results/malaria.csv', row.names=F)
