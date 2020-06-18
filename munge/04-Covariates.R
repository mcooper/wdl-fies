fs <- list.files('data/covars/results/', full.names=T)

covars <- expand.grid(list(GDLCODE=unique(gdl$GDLCODE), YEAR=1990:2030)) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3))

for (f in fs){
  d <- read.csv(f)
  print(f)
  print(intersect(names(d), names(covars)))
  #covars <- merge(covars, d, all.x=T, all.y=F)
  print(dim(covars))
}

cache('covars')

