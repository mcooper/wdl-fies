cty <- ne_countries(returnclass = 'sf') %>%
  select(iso_a3)

regions$iso_a3 <- as.character(regions$ISO3)

cty <- merge(cty, regions, on='iso_a3', all.x=T)

plt <- ggplot(cty) + 
  geom_sf(aes(fill=region)) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
plt

ggsave(plot = plt, filename = 'figures/regions.png', width=12, height=7)

cty$in_raw_data[is.na(cty$in_raw_data)] <- FALSE

plt <- ggplot(cty) + 
  geom_sf(aes(fill=in_raw_data)) + 
  coord_sf(crs='+proj=robin') + 
  scale_fill_manual(values=c("#bbbbbb", "#000245"), labels=c('No FIES Data', 'Has FIES Data')) + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(fill='')

plt

ggsave(plot = plt, filename = 'figures/data_countries.png', width=12, height=7)
