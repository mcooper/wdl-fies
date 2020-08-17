# #To Re-create munging environment
# setwd('~/wdl-fies');library(ProjectTemplate);load.project(list(cache_loading=F, data_loading=T, munging=F))
# #Then run files in munge/ in order

fs <- list.files('data/fies_surveys', full.names = T)

fies_raw <- data.frame()
for (f in fs){
  load(f)
  data$ISO3 <- substr(f, 19, 21)
  data$YEAR <- as.numeric(substr(f, 23, 26))

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
         ISO3 = case_when(ISO3=='KMH' ~ 'KHM',
                             TRUE ~ ISO3)) %>%
  select(-year)

#Read in GDL Code
gdl <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4') %>%
  rename(GDLCODE=GDLcode) %>%
  filter(!GDLCODE %in% c("COLr128", "GMBr101", "GMBr107", "INDr135", "NA"))

cache('fies_raw')
cache('gdl')
cache('regions')
