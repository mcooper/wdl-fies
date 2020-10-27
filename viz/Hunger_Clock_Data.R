
######################
# save output for poli
########################

sel <- preddat %>%
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
  spread(var, value)

write.csv(sel, 'figures/HC_2020-10-07.csv', row.names=F) #update date

