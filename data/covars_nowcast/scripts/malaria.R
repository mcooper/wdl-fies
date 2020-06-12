library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

fs <- list.files('data/covars_nowcast/rawdata/malaria', pattern='^2019_Global_P._Incidence_20...tif$', full.names = T, recursive=T)

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
         type=ifelse(grepl('Pf', timetype), 'mal_falciparum', 'mal_vivax')) %>%
  select(-timetype) %>%
  spread(type, incidence)

#Missing data at northern latitudes
e3$falciparum[is.na(e3$falciparum)] <- 0
e3$vivax[is.na(e3$vivax)] <- 0

#Add 2018 using 2017 values
e4 <- bind_rows(e3,
                e3 %>%
                  filter(YEAR==2017) %>%
                  mutate(YEAR=2018))

write.csv(e4, 'data/covars_nowcast/results/malaria.csv', row.names=F)
