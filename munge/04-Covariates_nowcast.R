fs <- list.files('data/covars_nowcast/results/', full.names=T)

covar_now <- expand.grid(list(GDLCODE=unique(gdl$GDLCODE), YEAR=1990:2020)) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3))

for (f in fs){
  d <- read.csv(f)
  covar_now <- merge(covar_now, d, all.x=T, all.y=F)
}

cache('covar_now')

