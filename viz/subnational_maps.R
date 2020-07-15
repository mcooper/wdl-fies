gdl_fies <- merge(gdl,
                  fies_subnat %>%
                    group_by(GDLCODE) %>%
                    summarize_if(is.numeric, mean))


ggplot() + 
  geom_sf(data=gdl, fill = '#999999', size=0.1) + 
  geom_sf(data=gdl_fies, aes(fill=urban_perc), size=0.1) +
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Percent of Population Living in Urban Areas in 2018')
ggsave('figures/subnational-Urban.png', width=12, height=7)

ggplot() + 
  geom_sf(data=gdl, fill = '#999999', size=0.1) + 
  geom_sf(data=gdl_fies, aes(fill=fies.mod), size=0.1) +
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Subnational Rate of Moderate Food Insecurity\n(Averaged over all years)')
ggsave('figures/subnational-moderate.png', width=12, height=7)

ggplot() + 
  geom_sf(data=gdl, fill = '#999999', size=0.1) + 
  geom_sf(data=gdl_fies, aes(fill=fies.sev), size=0.1) +
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Subnational Rate of Severe Food Insecurity\n(Averaged over all years)')
ggsave('figures/subnational-severe.png', width=12, height=7)
