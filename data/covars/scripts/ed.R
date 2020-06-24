library(zoo)
library(countrycode)
library(tidyverse)
library(sf)

options(stringsAsFactors=F)

sf <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')

#https://globaldatalab.org/shdi/download_files/
#https://globaldatalab.org/assets/2020/03/SHDI%20Complete%204.0%20%281%29.csv
past <-  read.csv('data/covars/rawdata/SHDI Complete 4.0.csv') %>%
  filter(GDLCODE %in% sf$GDLcode, year >=2010) %>%
  mutate(population=pop*1000000,
         ISO3=substr(GDLCODE, 1, 3)) %>%
  select(GDLCODE, ISO3,
         YEAR=year, admin_school_mean=msch, population) %>%
  group_by(ISO3, YEAR) %>%
  mutate(country_school_mean=weighted.mean(admin_school_mean, population)) %>%
  ungroup %>%
  mutate(admin_school_mean_diff = admin_school_mean - country_school_mean) %>%
  group_by(ISO3, GDLCODE) %>%
  complete(YEAR=2010:2030) %>%
  mutate(admin_school_mean_diff = ifelse(is.na(admin_school_mean_diff),
                                               mean(admin_school_mean_diff, na.rm=T),
                                               admin_school_mean_diff)) %>%
  select(GDLCODE, ISO3, YEAR, admin_school_mean_diff)


###################################################################
# Future Data
##################################################################
edu <- read.csv('data/covars/rawdata/ed/wcde_edu.csv', skip = 8) 
pop <- read.csv('data/covars/rawdata/ed/wcde_pop.csv', skip = 8)

edu <- merge(edu, pop, by = c("Area", "Year", "Age"), all.x = T) %>%
  filter(Year >= 2010 & Year <= 2030) %>%
  mutate(Population = Population*1000) %>% 
  mutate(ISO3 = countrycode(Area, origin = "country.name", destination = "iso3c")) %>%
  select(ISO3 , YEAR=Year, edu=Years, pop=Population) %>%
  group_by(ISO3, YEAR) %>%
  summarise(country_school_mean=sum(edu*pop)/sum(pop)) %>%
  group_by(ISO3) %>%
  complete(YEAR=2010:2030) %>%
  mutate(country_school_mean=na.approx(country_school_mean, na.rm=F)) %>%
  filter(!is.na(ISO3)) %>%
  fill(country_school_mean)

##############################################
# Combine
#############################################

comb <- merge(past, edu) %>%
  mutate(school_mean = country_school_mean + admin_school_mean_diff) %>%
  select(-admin_school_mean_diff, -country_school_mean)

write.csv(comb, 'data/covars/results/ed.csv', row.names=F)


