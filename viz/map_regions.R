cty <- ne_countries(scale = 50, returnclass = 'sf')

regions$iso_a3 <- regions$iso3

cty <- merge(cty, regions, all.x=T)

plt <- ggplot(cty) + 
  geom_sf(aes(fill=region)) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
plt

ggsave(plot = plt, filename = 'figures/regions.png', width=12, height=7)
