setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

######################
# save output for poli
########################

sel <- preddat %>%
  select(ISO3, YEAR, GDLCODE, fies.mod.pred) %>%
  merge(covars %>%
          select(ISO3, YEAR, GDLCODE, stunting, urban_perc, population)) %>%
  select(ISO3, YEAR, GDLCODE, stunting, urban_perc, fies.mod.pred, population) %>%
  filter(YEAR %in% c(2020, 2025, 2030)) %>%
  merge(u5.population, all.x=T, all.y=F) %>%
  mutate(u5pop.urban = urban_perc*u5_frac*population,
         u5pop.rural = (1 - urban_perc)*u5_frac*population,
         stunting.urban = stunting*u5pop.urban,
         stunting.rural = stunting*u5pop.rural,
         population.urban = population*urban_perc,
         population.rural = population*(1 - urban_perc),
         fies.urban = fies.mod.pred*population.urban,
         fies.rural = fies.mod.pred*population.rural) %>%
  select(-stunting, -urban_perc, -fies.mod.pred, -population, -u5_frac) %>%
  gather(var, value, -ISO3, -YEAR, -GDLCODE) %>%
  mutate(geo_area=ifelse(grepl('urban', var), 'Urban', 'Rural'),
         var=gsub('.rural|.urban', '', var),
         value = round(value)) %>%
  spread(var, value) %>%
  #Remove bad polygons
  filter(!GDLCODE %in% c("KNAt", "SMRt", "GBRr113", "GBRr114", "USAr152", "ECUr104",
                         "AZEr111", "AZEr110", "NZLr116", "CPVr106"))

gdl <- gdl %>%
  filter(GDLCODE %in% sel$GDLCODE)

write.csv(sel, paste0('figures/HC_', substr(Sys.time(), 1, 10), '_data.csv'), row.names=F) #update date
write_sf(gdl, paste0('figures/HC_', substr(Sys.time(), 1, 10), '_polys.geojson', driver='GeoJSON')
