setwd('~/wdl-fies');library(ProjectTemplate);load.project()

cty <- ne_countries(returnclass='sf')

iso3 <- unique(moddat$ISO3)

cty$FIES_DAT <- cty$iso_a3 %in% iso3
cty <- cty %>%
  filter(sovereignt != 'Antarctica')

ggplot(cty) + 
  geom_sf(aes(fill=FIES_DAT), show.legend=F, size=0.25) + 
  scale_fill_manual(values=rev(c("#000245", "#C0C0C0"))) + 
  coord_sf(crs='+proj=robin') +
  labs(title='Countries That Have FIES Microdata') + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('docs/img/FIES_Countries.pdf', width=6, height=4)
