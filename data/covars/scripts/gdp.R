library(zoo)
library(tidyverse)
library(countrycode)
library(sf)

########################################
# Define Functions
###################################
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

#######################################
#Read GDL GNI Data
#######################################
sf <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

#https://globaldatalab.org/shdi/download_files/
#https://globaldatalab.org/assets/2020/03/SHDI%20Complete%204.0%20%281%29.csv
dat <- read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLcode) %>%
  mutate(gni_percap_2011=gnic*1000,
         population=pop*1000000) %>%
  select(GDLCODE, 
         YEAR=year, life_expectancy=lifexp, gni_percap_2011, years_school_expected=esch,
         years_school_mean=msch, population, hdi=shdi)


all <- expand.grid(list(GDLCODE=unique(dat$GDLCODE), YEAR=1990:2018)) %>%
  merge(dat, all.x=T, all.y=F) %>%
  gather(var, value, -GDLCODE, -YEAR) %>%
  group_by(GDLCODE, var) %>%
  arrange(YEAR) %>%
  fill(value) %>%
  arrange(desc(YEAR)) %>%
  fill(value) %>%
  spread(var, value) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3))

##################################################################################
# Read in SSP2 Data
##############################################################################

ssp <- read.csv('data/covars/rawdata/ssp/SspDb_country_data_2013-06-12.csv') %>%
  filter(VARIABLE=='GDP|PPP', MODEL=='OECD Env-Growth', SCENARIO=='SSP2_v9_130325') %>%
  select(ISO3=REGION, X2010, X2015, X2020, X2025, X2030) %>%
  gather(YEAR, GDP, -ISO3) %>%
  mutate(YEAR = as.numeric(substr(YEAR, 2, 5))) %>%
  group_by(ISO3) %>%
  complete(YEAR=2010:2030) %>%
  mutate(GDP=na.approx(GDP)*1000000000) 

#Read in Subnational Population Data
pop <- read.csv('data/covars/results/urban-rural.csv')

############################################
#Read World Bank GNI and GDP Data
############################################
gdp <- read.csv('data/covars/rawdata/gdp/API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_1121050.csv', skip=4) %>%
  mutate(ISO3 = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, gdp, -ISO3) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(ISO3),
         YEAR >= 1990)

gni <- read.csv('data/covars/rawdata/gdp/API_NY.GNP.MKTP.KD_DS2_en_csv_v2_1129010.csv', skip=4) %>%
  mutate(ISO3 = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, gni, -ISO3) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(ISO3),
         YEAR >= 1990)

wb_comb <- merge(gdp, gni) %>%
  mutate(gdp_gni_ratio = gdp/gni) %>%
  arrange(desc(YEAR)) %>%
  group_by(ISO3) %>%
  fill(gdp_gni_ratio) %>%
  arrange(YEAR) %>%
  group_by(ISO3) %>%
  fill(gdp_gni_ratio)

#####################################################################
# Adjust SSP projections to match observed WB trajectories of GDP
####################################################################
compare <- merge(gdp %>%
                   rename(wb_gdp=gdp), 
                 ssp %>%
                   rename(ssp_gdp=GDP)) %>%
  mutate(ratio=wb_gdp/ssp_gdp) %>%
  merge(expand.grid(list(YEAR=2010:2030, ISO3=unique(ssp$ISO3))), all.y=T) %>%
  group_by(ISO3) %>%
  fill(ratio)

ssp <- ssp %>%
  merge(compare %>% select(ISO3, YEAR, ratio)) %>%
  mutate(GDP=GDP*ratio) %>%
  select(-ratio)

####################################################################################
# Combine Subnational GNI with National GDP/GNI Ratio to Proxy Subnational GDP
#################################################################################

all_comb <- merge(all, wb_comb) %>%
  mutate(gdp_percap = gni_percap_2011*gdp_gni_ratio,
         #Fill NAs with just GNI
         gdp_percap = ifelse(is.na(gdp_percap), gni_percap_2011, gdp_percap))

#################################################################################
# Determine admin shares of total gdp
########################################################################
gdp_proc <- all_comb %>%
  filter(YEAR >= 2010) %>%
  select(ISO3, YEAR, GDLCODE, gdp_percap, population) %>%
  mutate(admin_gdp = gdp_percap*population) %>%
  group_by(ISO3, YEAR) %>%
  mutate(cty_gdp = sum(admin_gdp)) %>%
  ungroup %>%
  mutate(admin_gdp_share = admin_gdp/cty_gdp) %>%
  group_by(ISO3, GDLCODE) %>%
  arrange(YEAR) %>%
  complete(YEAR=2010:2030) %>%
  mutate(admin_gdp_share = modelFuture(admin_gdp_share)) %>%
  ungroup %>%
  group_by(ISO3, YEAR) %>%
  mutate(admin_gdp_share_total = sum(admin_gdp_share)) %>%
  ungroup %>%
  mutate(admin_gdp_share = admin_gdp_share/admin_gdp_share_total) %>%
  select(ISO3, GDLCODE, YEAR, admin_gdp_share)

##Add future GDP projections
comb <- merge(ssp, gdp_proc, all=T) %>%
  filter(!is.na(GDLCODE) & !is.na(GDP)) %>%
  mutate(gdp = GDP*admin_gdp_share) %>%
  merge(pop %>%
          select(GDLCODE, ISO3, YEAR, population)) %>%
  mutate(gdp_percap = gdp/population,
         gdp_percap = ifelse(is.infinite(gdp_percap), NA, gdp_percap)) %>%
  select(ISO3, YEAR, GDLCODE, gdp_percap)

write.csv(comb, 'data/covars/results/gdp.csv', row.names=F)
