library(rnaturalearth)
library(tidyverse)
library(sf)
library(countrycode)
library(readxl)

sp <- read_sf('~/wdl-fies/data/GDL Shapefiles V4 0.005/', 'GDL Shapefiles V4 Edit')

d <- ne_countries(returnclass='sf') %>% 
  select(ISO3=iso_a3, region_wb) %>% 
  st_drop_geometry

gdp <- read_xlsx('~/wdl-fies/data/covars/rawdata/gdp/GlobalEconomicProspectsJune2020GDPgrowthdata.xlsx',
                 sheet='Statistical Appendix', skip=4) 
names(gdp)[1:9] <- c('bb', 'region', 'country', 'x', 'r2017', 'r2018', 'r2019', 'r2020', 'r2021')

#########################
# Get Country-level Data
#########################
cty <- gdp %>%
  select(country, matches('r20')) %>%
  filter(!is.na(country)) %>%
  mutate(ISO3 = countrycode(country, 'country.name', 'iso3c'))

cty$ISO3[cty$country == 'Kosovo'] <- 'KSV'

euro <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
euro <- euro[!euro %in% cty$ISO3]

eur <- data.frame(cty)[rep(which(cty$country == 'Euro Area'), 28), ]
eur$ISO3 <- euro

cty <- bind_rows(cty %>% filter(country != 'Euro Area'),
                 eur)
#################################
# Ger Region-level Dta
#################################
reg <- gdp %>%
  select(region, matches('r20')) %>%
  filter(!is.na(region))

eap_cty <- d$ISO3[d$region_wb == 'East Asia & Pacific' & !d$ISO3 %in% cty$ISO3]
#Drop AUS and NZL
eap_cty <- eap_cty[!eap_cty %in% c('AUS', 'NZL')]
eap <- data.frame(reg)[rep(which(reg$region == 'East Asia and Pacific'), length(eap_cty)), ]
eap$ISO3 <- eap_cty

eca_cty <- d$ISO3[d$region_wb == 'Europe & Central Asia' & !d$ISO3 %in% cty$ISO3]
eca <- data.frame(reg)[rep(which(reg$region == 'Europe and Central Asia'), length(eca_cty)), ]
eca$ISO3 <- eca_cty

lac_cty <- d$ISO3[d$region_wb == 'Latin America & Caribbean' & !d$ISO3 %in% cty$ISO3]
lac <- data.frame(reg)[rep(which(reg$region == 'Latin America and the Caribbean'), length(lac_cty)), ]
lac$ISO3 <- lac_cty

mena_cty <- d$ISO3[d$region_wb == 'Middle East & North Africa' & !d$ISO3 %in% cty$ISO3]
mena <- data.frame(reg)[rep(which(reg$region == 'Middle East and North Africa'), length(mena_cty)), ]
mena$ISO3 <- mena_cty

sa_cty <- d$ISO3[d$region_wb == 'South Asia' & !d$ISO3 %in% cty$ISO3]
sa <- data.frame(reg)[rep(which(reg$region == 'South Asia'), length(sa_cty)), ]
sa$ISO3 <- sa_cty

ssa_cty <- d$ISO3[d$region_wb == 'Sub-Saharan Africa' & !d$ISO3 %in% cty$ISO3]
ssa <- data.frame(reg)[rep(which(reg$region == 'Sub-Saharan Africa'), length(ssa_cty)), ]
ssa$ISO3 <- ssa_cty

#########################
# Combine and Write
##########################
comb <- bind_rows(cty, eap, eca, lac, mena, sa, ssa) %>%
  select(-region, -country) %>%
  na.omit

ISO3 <- unique(substr(sp$GDLcode, 1, 3))

#Rich countries and microstates are what remains
hic_cty <- ISO3[!ISO3 %in% comb$ISO3]
#Use HIC data from elsewhere in xlsx document
hic <- data.frame(r2017=2.4, r2018=2.2, r2019=1.7, r2020=-6.8, r2021=3.8, ISO3=hic_cty)

comb <- bind_rows(comb, hic) %>%
  filter(ISO3 != 'NA') %>%
  na.omit

all(ISO3 %in% comb$ISO3)

write.csv(comb, '~/wdl-fies/data/covars/rawdata/gdp/covid_impact.csv', row.names=F) 
