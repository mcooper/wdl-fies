library(tidyverse)
library(zoo)
library(rgdal)

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

  #Logit fails if any value is 1 or 0.
  #For my purposes, values very close to 1 and 0 will work
  #So set 1 = 0.99999
  
  if (any(var > 1, na.rm=T) | any(var < 0, na.rm=T)){
    stop("Value should be a proportion, cant be > 1 or < 0")
  }

  var[var == 1] <- 0.99999
  var[var == 0] <- 0.00001

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

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

u5 <- read.csv('data/covars/rawdata/ssp/SspDb_country_data_2013-06-12.csv') %>%
  filter(grepl('Aged0-4$', VARIABLE), 
         MODEL=='IIASA-WiC POP', 
         SCENARIO=='SSP2_v9_130115') %>%
  dplyr::select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  group_by(ISO3) %>%
  summarize_all(sum) %>%
  gather(YEAR, u5pop, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5))) %>%
  group_by(ISO3) %>%
  complete(YEAR=2010:2030) %>%
  mutate(u5pop = na.approx(u5pop)*1000000)

#Historic Population share by admin area
rates <- read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sp$GDLcode, year >= 2010) %>%
  mutate(gni_percap_2011=gnic*1000,
         population=pop*1000000) %>%
  dplyr::select(GDLCODE, 
         YEAR=year, population) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3)) %>%
  group_by(ISO3, YEAR) %>%
  mutate(cty_pop = sum(population)) %>%
  ungroup %>%
  mutate(admin_share = population/cty_pop) %>%
  group_by(ISO3, GDLCODE) %>%
  complete(YEAR=2010:2030) %>%
  mutate(admin_share = modelFuture(admin_share)) %>%
  group_by(ISO3, YEAR) %>%
  mutate(admin_shares_total = sum(admin_share)) %>%
  ungroup(admin_share = admin_share/admin_shares_total) %>%
  select(ISO3, GDLCODE, YEAR, admin_share)

#Now extrapolate these rates to the future
fut <-merge(rates, u5) %>%
  mutate(u5pop = u5pop*admin_share) %>%
  select(-admin_share)

write.csv(u5, 'data/u5_population.csv', row.names=F)
