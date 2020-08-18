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

cache('covars')
