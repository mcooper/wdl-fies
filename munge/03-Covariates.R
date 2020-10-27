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
         gdp_percap, gini, mal_falciparum, precip, hci, tave, 
         population, rural_perc, urban_perc, ws_share) %>%
  merge(regions %>%
          select(region=region_sdg, ISO3))

#########################################################################
# Missing entire variable at the country level.  
# Fill a missing countries with neighbors
# (Mostly Syria, South Sudan, North Korea, Kosovo)
#########################################################################

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
#Water Scarcity Share, School mean, GDP Percap, same as Tajikstan
#Malaria is 0
covars$mal_falciparum[covars$ISO3=='PRK'] <- 0

tjk <- covars %>%
  filter(ISO3=='TJK') %>%
  group_by(YEAR) %>%
  summarize(school_mean = weighted.mean(school_mean, w=population),
            gdp_percap = weighted.mean(gdp_percap, w=population),
            gini = weighted.mean(gini, w=population),
            ws_share = weighted.mean(ws_share, w=population))

covars$school_mean[covars$ISO3=='PRK'] <- tjk$school_mean
covars$gdp_percap[covars$ISO3=='PRK'] <- tjk$gdp_percap
covars$gini[covars$ISO3=='PRK'] <- tjk$gini
covars$ws_share[covars$ISO3=='PRK'] <- tjk$ws_share

###########################################################################
# Missing data at the region level
# Fill with mean of other regions
# (Mostly regions missing from GDL ontology - Jersey, Puerto Rico, Taiwan)
############################################################################
#Use country average for missing values
impute.mean <- function(x){replace(x, is.na(x), mean(x, na.rm=T))}

covars <- covars %>%
  group_by(ISO3, YEAR) %>%
  mutate_if(is.numeric, impute.mean)

##################################################################
# Missing dat chronologically
# Fill missing chronolical data with nearest temporal observation
# (This should mostly just affect GINI, missing in 2010)
###################################################################

temporal_clean <- function(x){
  if (all(is.na(x))){
    return(x)
  }
  na_locf(na_locf(x, option='locf'), option='nocb')
}

covars <- covars %>%
  group_by(GDLCODE) %>%
  arrange(YEAR) %>%
  mutate_if(is.numeric, temporal_clean)

######################################
# Scope remaining missing data
####################################
# covars$anyna <- rowSums(is.na(covars))
# covars[covars$anyna > 0, c("YEAR", "GDLCODE", "ISO3", "anyna")] -> bad
# 
# bad <- bad %>% 
#   spread(YEAR, anyna) %>% 
#   mutate(country=countrycode(ISO3, 'iso3c', 'country.name'))
# View(bad)
# 
# covars$anyna <- NULL

# Missing data for: Micronesia (Federated States of)", "Grenada", "St. Kitts & Nevis",
# "Andorra", "San Marino", "St. Lucia", "Antigua & Barbuda", "Liechtenstein",
# "Dominica", and "Kosovo"
#
# Drop all of these for now

covars <- na.omit(covars)

cache('covars')

