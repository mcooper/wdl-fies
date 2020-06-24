cty <- ne_countries(returnclass = 'sf') %>%
  select(iso_a3)

reg <- read.csv('regions3.csv')

reg$iso_a3 <- as.character(reg$ISO3)

cty <- merge(cty, reg, on='iso_a3')

plt <- ggplot(cty) + 
  geom_sf(aes(fill=region)) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
plt

ggsave(plot = plt, filename = 'figures/regions.png', width=12, height=7)
