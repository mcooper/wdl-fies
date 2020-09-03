setwd('~/wdl-fies')

library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

fs <- list.files('data/covars/rawdata/malaria', pattern='^2019_Global_P._Mortality_Rate_20...tif$', full.names = T, recursive=T)

s <- stack(fs)

e <- raster::extract(s, sp)

getColSumsOrNull <- function(x){
	if (is.null(dim(x))){
		return(data.frame(X2019_Global_Pf_Mortality_Rate_2017=NA))
	} else{
		return(colSums(x, na.rm=T))
	}
}

e2 <- lapply(e, getColSumsOrNull) %>%
	bind_rows

e3 <- e2 %>%
	mutate(GDLCODE=sp$GDLcode) %>%
  gather(timetype, mal_falciparum, -GDLCODE) %>%
  mutate(YEAR=as.numeric(substr(timetype, nchar(timetype) - 3, nchar(timetype)))) %>%
  select(-timetype)

#Missing data at northern latitudes
e3$mal_falciparum[is.na(e3$mal_falciparum)] <- 0

##################################################
# Now project to 2030 using methodlogy
##################################################

logit <- function(x){
  log(x/(1-x))
}

inv.logit <- function(x){
  1/(1 + exp(-x))
}

modelFuture <- function(var){
  #Model Future using methodology from Osgood-Zimmerman
  #Get year on year Average Rate of Change (AROC)
  #Get weighted average rate of change,
  #Then apply it to future

  #Var must be in order sequentially,
  # with only non-NAs preceeding NAs
  # must be a fraction (rate, or incidence)

  dat <- var[!is.na(var)]
  nas <- sum(is.na(var))

  roc <- mapply(function(y1, y2) logit(y2) - logit(y1),
         dat[1:(length(dat) -1)],
         dat[2:length(dat)])

  w <- (2:length(dat))/sum(2:length(dat)) 
  
  aroc <- sum(w*roc)

  pred <- inv.logit(logit(dat[length(dat)]) + aroc*(1:nas))
  
  pred[is.nan(pred)] <- 0

  res <- c(dat, pred)

  return(res)
}

e4 <- e3 %>%
  merge(expand.grid(list(GDLCODE=unique(e3$GDLCODE), YEAR=2000:2030)), all.y=T) %>%
  group_by(GDLCODE) %>%
  arrange(YEAR) %>%
  mutate(mal_falciparum=modelFuture(mal_falciparum/100000))

write.csv(e4, 'data/covars/results/malaria.csv', row.names=F)
