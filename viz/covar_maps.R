#Add 2018 data to shapefile
# setwd('~/wdl-fies');library(ProjectTemplate);load.project()

covar_map <- merge(gdl, covars %>% filter(YEAR %in% c(2015, 2020, 2025, 2030)))

exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland')

# Set up Model
v <- names(covar_map)[!names(covar_map) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'Urban', 'Rural', 'fies.sev', 'fies.mod',
                                            'YEAR', 'rural_perc', 'region', 
                                            'geometry',
                                            names(covar_map)[grepl('region', names(covar_map))],
                                            exc)]

covar_map$mal_falciparum <- covar_map$mal_falciparum*1000000

for (n in v){
  covar_map$mapvar <- covar_map[ , n, drop=TRUE]

  ggplot() + 
    geom_sf(data=covar_map, aes(fill=mapvar), size=0.05) +
    labs(fill='',
         title='') + 
    scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", 
                                   "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                   "#f36c44", "#a01c44")) +
    coord_sf(crs='+proj=robin') +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(~ YEAR, nrow=2, ncol=2)

  ggsave(paste0('docs/img/covars/', n, '.png'), width=7, height=5)
}

#Overwrite topographic ruggedness with just one year
n <- 'ruggedness'
covar_map$mapvar <- covar_map[ , n, drop=TRUE]
covar_map <- covar_map %>% filter(YEAR == 2015)
ggplot() + 
  geom_sf(data=covar_map, aes(fill=mapvar), size=0.05) +
  labs(fill='',
       title='') + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", 
                                 "#e4ea9a", "#fbf8c0", "#fce08a", "#faae61", 
                                 "#f36c44", "#a01c44")) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0('docs/img/covars/', n, '.png'), width=7, height=5)
