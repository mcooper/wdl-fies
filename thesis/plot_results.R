
###############
# Time Series
###############

# time line and stacked
totals <- preddat %>%
  filter(YEAR > 2010) %>%
  #group_by(YEAR) %>%
  group_by(YEAR, region) %>%
  summarize(mod.total=sum(fies.mod.pred * (population), na.rm=T),
            sev.total=sum(fies.sev.pred * (population), na.rm=T)) %>%
  gather(var, value, -YEAR, -region)


ggplot(totals %>% filter(var=='mod.total')) +
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Moderate or Severe Food Insecurity, By Continent, Stacked\n(", m,")")) +
  theme_bw()
ggsave(paste0('figures/', m,'/Time.Mod.Stack_', m,'.png'), width=7, height=5)

ggplot(totals %>% filter(var=='mod.total')) +
  geom_line(aes(x=YEAR, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Moderate or Severe Food Insecurity, By Continent\n(", m,")")) +
  theme_bw()
ggsave(paste0('figures/', m,'/Time.Mod.Lines_', m,'.png'), width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Severe Food Insecurity, By Continent, Stacked\n(", m,")")) +
  theme_bw()
ggsave(paste0('figures/',m,'/Time.Sev.Stack_',m,'.png'), width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_line(aes(x=YEAR, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Severe Food Insecurity, By Continent\n(", m,")")) +
  theme_bw()
ggsave(paste0('figures/',m,'/Time.Sev.Lines_',m,'.png'), width=7, height=5)



# absolute rural or urban
ru.abs <- preddat %>%
  filter(YEAR > 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.rural=sum(fies.mod.pred * (population*rural_perc), na.rm=T),
            mod.urban=sum(fies.mod.pred * (population*urban_perc), na.rm=T),
            sev.rural=sum(fies.sev.pred * (population*rural_perc), na.rm=T),
            sev.urban=sum(fies.sev.pred * (population*urban_perc), na.rm=T)) %>%
  gather(var, value, -YEAR) %>%
  mutate(out = ifelse(grepl('mod', var), 'Mod', 'Sev'),
         var = ifelse(grepl('rural', var), 'Rural', 'Urban'))


ggplot(ru.abs %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Moderate or Severe Food Insecurity, By Rural/Urban\n(", m,")")) +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave(paste0('figures/', m,'/Time.Mod.Lines.RurUrb_', m,'.png'), width=8, height=5)

ggplot(ru.abs %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Number With Severe Food Insecurity, By Rural/Urban\n(", m,")")) +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave(paste0('figures/', m,'/Time.Sev.Lines.RurUrb_', m,'.png'), width=8, height=5)



# proportion rural or urban
ru.prop <- preddat %>%
  mutate(pop.rural=population*rural_perc,
         pop.urban=population*urban_perc,
         mod.rural=fies.mod.pred * pop.rural,
         mod.urban=fies.mod.pred * pop.urban,
         sev.rural=fies.sev.pred * pop.rural,
         sev.urban=fies.sev.pred * pop.urban) %>%
  group_by(YEAR) %>%
  summarize_at(vars(matches('urban$|rural$')), sum) %>%
  mutate(sev.rural=sev.rural/pop.rural,
         sev.urban=sev.urban/pop.urban,
         mod.rural=mod.rural/pop.rural,
         mod.urban=mod.urban/pop.urban) %>%
  select(-matches('pop')) %>%
  gather(var, value, -YEAR) %>%
  mutate(out = ifelse(grepl('mod', var), 'Mod', 'Sev'),
         var = ifelse(grepl('rural', var), 'Rural', 'Urban'))


ggplot(ru.prop %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Proportion With Moderate or Severe Food Insecurity, By Rural/Urban\n(", m,")")) +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave(paste0('figures/', m,'/Time.Mod.Lines.Prop.RurUrb_', m,'.png'), width=8, height=5)

ggplot(ru.prop %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title=paste0("Proportion With Severe Food Insecurity, By Rural/Urban\n(", m,")")) +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave(paste0('figures/', m,'/Time.Sev.Lines.Prop.RurUrb_', m,'.png'), width=8, height=5)




###########
# Map
###########

mapdat <- merge(gdl, 
                preddat %>% 
                  filter(YEAR %in% c(2020, 2025, 2030)) %>%
                  select(GDLCODE, YEAR, fies.mod.pred, fies.sev.pred), 
                all.x=T, all.y=F) %>%
  merge(totals %>%
          filter(YEAR %in% c(2020, 2025, 2030)) %>%
          group_by(YEAR, var) %>%
          summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
          spread(var, value) %>%
          mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total, 
                                   '\nIn Moderate or Severe Food Insecurity'),
                 sev.YEAR = paste0(YEAR, ':\n', sev.total, 
                                   '\nIn Severe Food Insecurity')))

countries <- ne_countries(returnclass='sf')
ggplot(mapdat) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title=paste0("Rate of Moderate or Severe Food Insecurity Under SSP2 (", m,")"),
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave(paste0('figures/', m,'/Map.Mod_', m,'.png'), width=10, height=12)

ggplot(mapdat) +
  geom_sf(aes(fill=fies.sev.pred), color=NA) +
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0",
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) +
  geom_sf(data=countries, color='#000000', fill=NA) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(title=paste0("Rate of Severe Food Insecurity Under SSP2 (", m,")"),
       fill='') +
  facet_grid(sev.YEAR ~ .)
ggsave(paste0('figures/', m,'/Map.Sev_', m,'.png'), width=10, height=12)



# individual years (only moderat)
ggplot(mapdat %>% filter(YEAR == 2020)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title=paste0("Rate of Moderate or Severe Food Insecurity Under SSP2 in 2020 (", m,")"),
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave(paste0('figures/', m,'/Map.Mod.20_', m,'.png'), width=10, height=5)

ggplot(mapdat %>% filter(YEAR == 2025)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title=paste0("Rate of Moderate or Severe Food Insecurity Under SSP2 in 2025 (", m,")"),
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave(paste0('figures/', m,'/Map.Mod.25_', m,'.png'), width=10, height=5)

ggplot(mapdat %>% filter(YEAR == 2030)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title=paste0("Rate of Moderate or Severe Food Insecurity Under SSP2 in 2030 (", m,")"),
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave(paste0('figures/', m,'/Map.Mod.30_', m,'.png'), width=10, height=5)

