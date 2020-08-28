fs <- list.files('data/covars/results/', full.names=T)

covars <- expand.grid(list(GDLCODE=unique(gdl$GDLCODE), YEAR=2010:2030)) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3))

for (f in fs){
  d <- read.csv(f)
  covars <- merge(covars, d, all.x=T, all.y=F)
}

#Select ony vars that have future data
covars <- covars %>%
  select(YEAR, ISO3, GDLCODE, stunting, wasting, school_mean, ruggedness,
         gdp_percap, gini, mal_falciparum, mal_vivax, mean_annual_precip, hci, tavg, 
         population, rural_perc, urban_perc, wci_index, builtup, cropland, crops_prod,
         forest, livestock, pasture)

##############################
#Post-hoc data matching
################################

########  Syria  #############
#HCI is the same as Yemen
yemen_hci <- unique(covars$hci[covars$ISO3=='YEM'])
n_admin1 <- length(unique(covars$GDLCODE[covars$ISO3=='SYR']))
covars$hci[covars$ISO3=='SYR'] <- rep(yemen_hci, each=n_admin1)

######## South Sudan #############
#GDP PerCap and GINI are the same as Central African Republic
caf_gdp_percap <- covars %>% 
  filter(ISO3=='CAF') %>%
  group_by(YEAR) %>%
  summarize(gdp_percap = weighted.mean(gdp_percap, w=population)) %>%
  .$gdp_percap
caf_gini <- unique(covars$gini[covars$ISO3=='CAF'])
n_admin1 <- length(unique(covars$GDLCODE[covars$ISO3=='SSD']))
covars$gdp_percap[covars$ISO3=='SSD'] <- rep(caf_gdp_percap, each=n_admin1)
covars$gini[covars$ISO3=='SSD'] <- rep(caf_gini, each=n_admin1)

######## North Korea #############
#WCI, School mean, GDP Percap, same as Tajikstan
#Malaria is 0
covars$mal_falciparum[covars$ISO3=='PRK'] <- 0
covars$mal_vivax[covars$ISO3=='PRK'] <- 0

tjk <- covars %>%
  filter(ISO3=='TJK') %>%
  group_by(YEAR) %>%
  summarize(school_mean = weighted.mean(school_mean, w=population),
            gdp_percap = weighted.mean(gdp_percap, w=population),
            gini = weighted.mean(gini, w=population),
            wci_index = weighted.mean(wci_index, w=population))

covars$school_mean[covars$ISO3=='PRK'] <- tjk$school_mean
covars$gdp_percap[covars$ISO3=='PRK'] <- tjk$gdp_percap
covars$gini[covars$ISO3=='PRK'] <- tjk$gini
covars$wci_index[covars$ISO3=='PRK'] <- tjk$wci_index

cache('covars')
