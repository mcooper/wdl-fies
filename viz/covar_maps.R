#Add 2018 data to shapefile

covar_map <- merge(gdl, covar_now %>% filter(YEAR==2018))

v <- names(covar_map)[!names(covar_map) %in% c('GDLCODE', 'constant', 'iso_code', 'country',
                                               'region', 'YEAR', 'ISO3')]

countries <- ne_countries(returnclass='sf')

for (n in v){
  covar_map$mapvar <- covar_map[ , n, drop=TRUE]

  ggplot() + 
    geom_sf(data=covar_map, aes(fill=mapvar)) +
    geom_sf(data=countries, fill='transparent') + 
    labs(fill=n,
         title=n) + 
    scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", 
                                   "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                   "#f36c44", "#a01c44")) +
    coord_sf(crs='+proj=robin') +
    theme_void() +
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5))

  ggsave(paste0('figures/covar_maps/', n, '.png'), width=10, height=8)
}
