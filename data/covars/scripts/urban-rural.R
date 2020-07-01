library(zoo)
library(raster)
library(rgdal)
library(tidyverse)
library(imputeTS)

######################################
# Define Functions
######################################

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

########################################################
# Read in data
###############################################

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')


#Historic Population by admin area
ref_past <- read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sp$GDLcode, year >= 2010) %>%
  mutate(gni_percap_2011=gnic*1000,
         population=pop*1000000) %>%
  dplyr::select(GDLCODE, 
         YEAR=year, population)

#Future Population by country
ref_fut <- read.csv('data/covars/rawdata/ssp/SspDb_country_data_2013-06-12.csv') %>%
  filter(VARIABLE=='Population', MODEL=='OECD Env-Growth', SCENARIO=='SSP2_v9_130325') %>%
  dplyr::select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  gather(YEAR, population, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5))) %>%
  group_by(ISO3) %>%
  complete(YEAR=2010:2030) %>%
  mutate(population = na.approx(population)*1000000)

######################################################
#Get each admin areas share of national tot, and model how that share has changed over time
####################################################
rates <- ref_past %>%
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
fut <-merge(rates, ref_fut) %>%
  mutate(population = population*admin_share) %>%
  select(-admin_share)

#######################################################
# Now extract future shares urban and rural by admin area
#############################################################

#IF extacting from raw rastere
#FROM https://www.worldclim.org/data/worldclim21.html
ur <- stack(list.files('data/covars/rawdata/urban-rural', full.names=T))

e <- raster::extract(ur, sp, method='simple', fun=sum, na.rm=T,
                                    sp=TRUE, df=TRUE)

res <- e@data %>%
  dplyr::select(GDLCODE=GDLcode, matches('ssp2')) %>%
  gather(Var, Pop, -GDLCODE) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3),
         YEAR=as.numeric(substr(Var, nchar(Var) - 3, nchar(Var))),
         UrbRur=ifelse(grepl("rur", Var), "Rural", "Urban"),
         Pop=Pop*1000) %>%
  dplyr::select(-Var) %>%
  spread(UrbRur, Pop) %>%
  group_by(ISO3, GDLCODE) %>%
  complete(YEAR=2010:2030) %>%
  mutate(Rural=na.approx(Rural),
        Urban=na.approx(Urban),
        Total=Rural + Urban,
        Rural_Perc = Rural/Total,
        Urban_Perc = Urban/Total)

r <- merge(res, fut) %>%
  mutate(rural = Rural_Perc*population,
         urban = Urban_Perc*population) %>%
  select(GDLCODE, ISO3, YEAR, population, rural, urban)


#For missing values in Vanuatu, use country means
r <- r %>%
  group_by(YEAR) %>%
  mutate(urban = ifelse(is.na(urban) & grepl('VUT', GDLCODE), 
                        mean(urban[grepl('VUT', GDLCODE)], na.rm=T),
                             urban),
         rural = ifelse(is.na(rural) & grepl('VUT', GDLCODE),
                        mean(rural[grepl('VUT', GDLCODE)], na.rm=T),
                        rural))

#Round to integers
r <- r %>%
  mutate_if(is.numeric, round) %>%
  mutate(rural_perc = rural/population,
         urban_perc = urban/population) %>%
  select(-rural, -urban)

write.csv(r, 'data/covars/results/urban-rural.csv', row.names=F)
    