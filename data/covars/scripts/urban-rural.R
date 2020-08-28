setwd('~/wdl-fies')

library(zoo)
library(raster)
library(rgdal)
library(tidyverse)
library(imputeTS)
library(countrycode)

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

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

#Historic Population by admin area
ref_past <- read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sp$GDLcode, year >= 2010) %>%
  mutate(gni_percap_2011=gnic*1000,
         population=pop*1000000) %>%
  dplyr::select(GDLCODE, 
         YEAR=year, population) %>%
  bind_rows(data.frame(GDLCODE='PRKt', #Add data for North Korea
                       YEAR=2010:2018,
                       population=seq(24.55e6, #Rough estimates from google
                                      25.55e6,
                                      length.out=9)))

#Future Population by country
ref_fut <- read.csv('data/covars/rawdata/ssp/SspDb_country_data_2013-06-12.csv') %>%
  filter(VARIABLE=='Population', MODEL=='OECD Env-Growth', SCENARIO=='SSP2_v9_130325') %>%
  dplyr::select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  gather(YEAR, population, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5))) %>%
  group_by(ISO3) %>%
  complete(YEAR=2010:2030) %>%
  mutate(population = na.approx(population)*1000000)

  #Extrapolate for South Sudan using rates for CAR
  #Extrapolate for North Korea using rates for Tajikstan
  SSD_ref <- sum(ref_past[grepl("SSD", ref_past$GDLCODE ) & ref_past$YEAR == 2010, 'population'])
  PRK_ref <- sum(ref_past[grepl("PRK", ref_past$GDLCODE ) & ref_past$YEAR == 2010, 'population'])

  SSD <- ref_fut %>% 
    filter(ISO3=='CAF') %>% 
    mutate(rate=population/min(population),
           population=SSD_ref*rate,
           ISO3='SSD') %>%
    select(-rate)

  PRK <- ref_fut %>% 
    filter(ISO3=='TJK') %>% 
    mutate(rate=population/min(population),
           population=PRK_ref*rate,
           ISO3='PRK') %>%
    select(-rate)

  ref_fut <- bind_rows(ref_fut, SSD, PRK)

#Get actual trends from 2010-2019
actual <- read.csv('data/covars/rawdata/API_SP.POP.TOTL_DS2_en_csv_v2_1308146.csv', skip=4) %>%
  dplyr::select(-Country.Name, -Indicator.Name, -Indicator.Code) %>%
  gather(YEAR, pop_actual, -Country.Code) %>%
  mutate(ISO3 = countrycode(Country.Code, 'wb', 'iso3c'),
         YEAR = as.numeric(substr(YEAR, 2, 5))) %>%
  filter(!is.na(ISO3), YEAR > 2009) %>%
  dplyr::select(-Country.Code)

#Get country-level ratio of actual trends to predicted (from 2010) trends
compare <- merge(ref_fut, actual, all.x=T, all.y=F) %>%
  mutate(off = pop_actual/population) %>%
  #Add space for years to 2030
  merge(expand.grid(list(YEAR=2010:2030, ISO3=unique(ref_fut$ISO3))), all.y=T) %>%
  group_by(ISO3) %>%
  fill(off)

new_ref_fut <- merge(ref_fut, compare, all.x=T, all.y=F) %>%
  mutate(off = ifelse(is.na(off), 1, off),
         population = population*off) %>%
  dplyr::select(-off, -pop_actual)

##############################################3
# Compare trajectories from:
#  - GDL
#  - World Bank
#  - Future SSP Projections
#  - World Bank-Adjusted Future SSP Projections
################################################

# ref_past_sum <- ref_past %>%
#   group_by(YEAR) %>%
#   summarize(pop = sum(population, na.rm=T)) %>%
#   mutate(ref='past')
# 
# ref_fut_sum <- ref_fut %>%
#   group_by(YEAR) %>%
#   summarize(pop = sum(population, na.rm=T)) %>%
#   mutate(ref='future')
# 
# ref_wb_sum <- actual %>%
#   group_by(YEAR) %>%
#   summarize(pop = sum(pop_actual, na.rm=T)) %>%
#   mutate(ref='wb')
# 
# new_ref_fut <- new_ref_fut %>%
#   group_by(YEAR) %>%
#   summarize(pop = sum(population, na.rm=T)) %>%
#   mutate(ref='new_fut')
# 
# all <- bind_rows(ref_past_sum, ref_fut_sum, ref_wb_sum, new_ref_fut)
# 
# ggplot(all) + 
#   geom_line(aes(x=YEAR, y=pop, color=ref))

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
  dplyr::select(ISO3, GDLCODE, YEAR, admin_share)

#Now extrapolate these rates to the future
fut <-merge(rates, new_ref_fut) %>%
  mutate(population = population*admin_share) %>%
  dplyr::select(-admin_share)


#######################################################
# Now extract future shares urban and rural by admin area
#############################################################

#IF extacting from raw rastere
#FROM https://www.worldclim.org/data/worldclim21.html
ur <- stack(list.files('data/covars/rawdata/urban-rural', full.names=T, pattern='tif$'))

#e <- raster::extract(ur, sp, method='simple', fun=sum, na.rm=T,
#                      sp=TRUE, df=TRUE)
#
#e <- e@data

e <- read.csv('data/covars/rawdata/urban-rural/e.csv')

res <- e %>%
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
  dplyr::select(GDLCODE, ISO3, YEAR, population, rural, urban)


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
  dplyr::select(-rural, -urban)

write.csv(r, 'data/covars/results/urban-rural.csv', row.names=F)
    
