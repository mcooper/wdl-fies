ggplot(fies_cty) + 
  geom_point(aes(x=fies.sev.3yr.fao,
                 y=fies.sev.3yr)) + 
  labs(x='Published Estimates of Severe Food Insecurity',
       y='Manually Calculated Estimates of Severe Food Insecurity')
ggsave('figures/FAO Compare - Severe.png')

ggplot(fies_cty) + 
  geom_point(aes(x=fies.mod.3yr.fao,
                 y=fies.mod.3yr)) + 
  labs(x='Published Estimates of Moderate Food Insecurity',
       y='Manually Calculated Estimates of Moderate Food Insecurity')
ggsave('figures/FAO Compare - Moderate.png')
