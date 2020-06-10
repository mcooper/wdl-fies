
fs <- list.files('data/fies_surveys', full.names = T)

fies_raw <- data.frame()
for (f in fs){
  load(f)
  data$iso3 <- substr(f, 19, 21)
  data$year <- as.numeric(substr(f, 23, 26))

  fies_raw <- rbind(fies_raw, data)  
}

fies_raw <- fies_raw %>%
  #Convert data to numeric
  mutate(WORRIED = as.numeric(WORRIED),
         HEALTHY = as.numeric(HEALTHY),
         FEWFOOD = as.numeric(FEWFOOD),
         SKIPPED = as.numeric(SKIPPED),
         ATELESS = as.numeric(ATELESS),
         RUNOUT = as.numeric(RUNOUT),
         HUNGRY = as.numeric(HUNGRY),
         WHLDAY = as.numeric(WHLDAY),
         #Looks like cambodia was mis-coded?
         iso3 = case_when(iso3=='KMH' ~ 'KHM',
                             TRUE ~ iso3))

cache('fies_raw')
