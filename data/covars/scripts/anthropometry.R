library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

stunting <- stack(list.files('data/covars/rawdata/stunting', full.names=T))
wasting <- stack(list.files('data/covars/rawdata/wasting', full.names=T))
      
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

ep <- em %>%
  merge(expand.grid(list(GDLCODE=unique(em$GDLCODE), YEAR=2000:2030)), all.y=T) %>%
  group_by(GDLCODE) %>%
  arrange(YEAR) %>%
  mutate(stunting=modelFuture(stunting),
         wasting=modelFuture(wasting))

write.csv(ep, 'data/covars/results/anthro_vars.csv', row.names=F)
