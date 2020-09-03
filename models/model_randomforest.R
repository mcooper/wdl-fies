#######################################################
# use random forest with ssp covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

# setwd('~/wdl-fies'); library(projecttemplate); load.project()

# read in data
moddat <- merge(fies_subnat, 
                covars, 
                all.x=t, all.y=f) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame

exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland')

# set up model
vars <- names(moddat)[!names(moddat) %in% c('iso3', 'gdlcode', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'year', 'rural_perc', 'region',
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc)]

moddat <- moddat %>%
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))

## mod+sev
# tune parameters
rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(4/5))),
                          trace = t,
                          dobest = t); rf.tune.mod$optimal

# model
rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.mod$optimal[[2]],
                nodesize = rf.tune.mod$optimal[[1]],
                do.trace = true)

#get predications
moddat$fies.mod.pred <- inv.logit(predict(rf.mod, moddat)$predicted)
preddat$fies.mod.pred <- inv.logit(predict(rf.mod, preddat)$predicted)

## sev
# tune parameters
rf.tune.sev <- tune.rfsrc(formula = as.formula(paste("fies.sev", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(4/5))),
                          trace = t,
                          dobest = t); rf.tune.sev$optimal
# model
rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = true)

# predictions
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))

moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))

############################
# visualize results
############################

######################
# save output for poli
########################

sel <- preddat %>%
  select(iso3, year, gdlcode, stunting, urban_perc, fies.mod.pred, population) %>%
  rename(fies.mod.pred = "fies.mod.pred") %>%
  filter(year %in% c(2020, 2025, 2030)) %>%
  merge(u5.population) %>%
  mutate(u5pop.urban = urban_perc*u5_frac*population,
         u5pop.rural = (1 - urban_perc)*u5_frac*population,
         stunting.urban = stunting*u5pop.urban,
         stunting.rural = stunting*u5pop.rural,
         population.urban = population*urban_perc,
         population.rural = population*(1 - urban_perc),
         fies.urban = fies.mod.pred*population.urban,
         fies.rural = fies.mod.pred*population.rural) %>%
  select(-stunting, -urban_perc, -fies.mod.pred, -population, -u5_frac) %>%
  gather(var, value, -iso3, -year, -gdlcode) %>%
  mutate(geo_area=ifelse(grepl('urban', var), 'urban', 'rural'),
         var=gsub('.rural|.urban', '', var),
         value = round(value)) %>%
  spread(var, value)

write.csv(sel, 'figures/fies.mod.results_randomforest.csv', row.names=f)


# #growth rate
# growth <- preddat %>%
#   filter(year > 2010) %>%
#   group_by(year) %>%
#   summarize(fies_total = sum(fies.mod.pred * (population), na.rm=t)) %>%
#   mutate(rate = ((fies_total-lag(fies_total))/lag(fies_total))*100,
#          abs = fies_total-lag(fies_total),
#          persec = abs/31536000) #31536000 second per 365 days/1 year
# 
# #we need only 2020, 2025 and 2030
# #should we do some sort of splining between these years? or do you have some more ideas?
# #your call
# 
# #growth rate
# growth <- preddat %>%
#   filter(year %in% c(2020, 2025, 2030)) %>%
#   group_by(year) %>%
#   summarize(fies_total = sum(fies.mod.pred * (population), na.rm=t)) %>%
#   mutate(abs = (fies_total-lag(fies_total))/5,
#          persec = abs/(31536000)) #31536000 second per 365 days/1 year

#####################
# time series
###############

#get totals by year
totals <- preddat %>%
  filter(year > 2010) %>%
  #group_by(year) %>%
  group_by(year, region) %>%
  summarize(mod.total=sum(fies.mod.pred * (population), na.rm=t),
            sev.total=sum(fies.sev.pred * (population), na.rm=t)) %>%
  gather(var, value, -year, -region)


ggplot(totals %>% filter(var=='mod.total')) +
  geom_area(aes(x=year, y=value, fill=region), position='stack') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='year', y="number food insecure",
       title="number with moderate or severe food insecurity, by continent, stacked\n(random forest)") +
  theme_bw()
ggsave('figures/randomforest/time.mod.stack_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='mod.total')) +
  geom_line(aes(x=year, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='year', y="number food insecure",
       title="number with moderate or severe food insecurity, by continent\n(random forest)") +
  theme_bw()
ggsave('figures/randomforest/time.mod.lines_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_area(aes(x=year, y=value, fill=region), position='stack') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='year', y="number food insecure",
       title="number with severe food insecurity, by continent, stacked\n(random forest)") +
  theme_bw()
ggsave('figures/randomforest/time.sev.stack_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_line(aes(x=year, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='year', y="number food insecure",
       title="number with severe food insecurity, by continent\n(random forest)") +
  theme_bw()
ggsave('figures/randomforest/time.sev.lines_randomforest.png', width=7, height=5)


############################
# make map
##############################
mapdat <- merge(gdl,
                preddat %>% 
                  filter(year %in% c(2020, 2025, 2030)) %>%
                  select(gdlcode, year, fies.mod.pred, fies.sev.pred), 
                all.x=t, all.y=f) %>%
  merge(totals %>%
          filter(year %in% c(2020, 2025, 2030)) %>%
          group_by(year, var) %>%
          summarize(value=prettynum(sum(value), scientific=f, big.mark=',')) %>%
          spread(var, value) %>%
          mutate(mod.year = paste0(year, ':\n', mod.total, 
                                   '\nin moderate or severe food insecurity'),
                 sev.year = paste0(year, ':\n', sev.total, 
                                   '\nin severe food insecurity')))

countries <- ne_countries(returnclass='sf')
ggplot(mapdat) + 
  geom_sf(aes(fill=fies.mod.pred), color=na) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=na) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='rate of moderate or severe food insecurity under ssp2 (random forest)',
       fill='') + 
  facet_grid(mod.year ~ .)
ggsave('figures/randomforest/ssp2_map_moderate_randomforest.png', width=10, height=12)

ggplot(mapdat) +
  geom_sf(aes(fill=fies.sev.pred), color=na) +
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0",
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) +
  geom_sf(data=countries, color='#000000', fill=na) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(title='rate of severe food insecurity under ssp2 (random forest)',
       fill='') +
  facet_grid(sev.year ~ .)
ggsave('figures/randomforest/ssp2_map_severe_randomforest.png', width=10, height=12)


##################################
# assess residuals
##################################
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  labs(title='model performance (random forest)',
       caption=paste0('mean absolute error: ',  round(mae, 4),
                      '\nr-squared: ', round(r2, 4)),
       x='observed rates of mod+sev food insecurity',
       y='modeled rate of mod+sev food insecurity')	
ggsave('figures/randomforest/ssp2_mod_residuals_randomforest.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
  labs(title='model performance (random forest)',
       caption=paste0('mean absolute error: ',  round(mae, 4),
                      '\nr-squared: ', round(r2, 4)),
       x='observed rates of sev food insecurity',
       y='modeled rate of sev food insecurity')	
ggsave('figures/randomforest/ssp2_sev_residuals_randomforest.png', width=5, height=5)


##################################
# plot scaled covariates
#####################################


# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                ntree = 500)

# variable importance
png('figures/randomforest/ssp2_mod_vimp_randomforest.png', width = 1000, height = 500, units="px")
plot(vimp(rf.mod))
dev.off()

png('figures/randomforest/ssp2_sev_vimp_randomforest.png', width = 1000, height = 500, units="px")
plot(vimp(rf.sev))
dev.off()


# variable effect
png('figures/randomforest/ssp2_mod_coefs_randomforest.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.mod, sorted = t)
dev.off()

png('figures/randomforest/ssp2_sev_coefs_randomforest.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.sev, sorted = t)
dev.off()



############################
# ifad call
############################
#graphs
# ggplot(totals %>% filter(var=='mod.total')) +
#   geom_line(aes(x=year, y=value, color=region), size=1) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0), labels = scales::comma) +
#   labs(x='year', y="number food insecure",
#        title="number with moderate or severe food insecurity, by continent") + 
#   theme_bw() +
#   theme(legend.title=element_blank())
# ggsave('figures/ifad/time.mod.lines_randomforest.png', width=8, height=5)


#rural/urban
totals <- preddat %>%
  filter(YEAR > 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.rural=sum(fies.mod.pred * (population*rural_perc), na.rm=T),
            mod.urban=sum(fies.mod.pred * (population*urban_perc), na.rm=T),
            sev.rural=sum(fies.sev.pred * (population*rural_perc), na.rm=T),
            sev.urban=sum(fies.sev.pred * (population*urban_perc), na.rm=T)) %>%
  gather(var, value, -YEAR) %>%
  mutate(out = ifelse(grepl('mod', var), 'Mod', 'Sev'),
         var = ifelse(grepl('rural', var), 'Rural', 'Urban'))

ggplot(totals %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/randomforest/Time.Mod.Lines.RurUrb_randomforest.png', width=8, height=5)


ggplot(totals %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/randomforest/Time.Sev.Lines.RurUrb_randomforest.png', width=8, height=5)

#Proportion Urban or Rural
prop <- preddat %>%
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

ggplot(prop %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Proportion of Urban and Rural People With Moderate or Severe Food Insecurity") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/randomforest/Time.Mod.Lines.Prop.RurUrb_randomforest.png', width=8, height=5)


ggplot(prop %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/randomforest/Time.Sev.Lines.Prop.RurUrb_randomforest.png', width=8, height=5)


#Map Individual Years
ggplot(mapdat %>% filter(YEAR == 2020)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2 in 2020',
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave('figures/randomforest/SSP2_Moderate_2020_randomforest.png', width=10, height=5)

ggplot(mapdat %>% filter(YEAR == 2025)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2 in 2025',
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave('figures/randomforest/SSP2_Moderate_2025_randomforest.png', width=10, height=5)

ggplot(mapdat %>% filter(YEAR == 2030)) + 
  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2 in 2030',
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave('figures/randomforest/SSP2_Moderate_2030_randomforest.png', width=10, height=5)


