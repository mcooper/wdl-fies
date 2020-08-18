#setwd('~/wdl-fies');library(ProjectTemplate);load.project()
cty <- ne_countries(returnclass = 'sf') %>%
  select(ISO3=iso_a3, region=region_wb) %>%
  filter(region != 'Antarctica')

plt <- ggplot(cty) + 
  geom_sf(aes(fill=region)) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)
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
