#######################################################
# Use Random Forest with SSP covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

# setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

covars <- covars %>% merge(ne_countries()@data %>%
                           select(ISO3=iso_a3, region=region_wb))

ne_countries()@data %>%
  select(ISO3=iso_a3, region=region_wb)

#Just to fix Siberia
covars$wci_index[covars$GDLCODE=='RUSr108'] <- 1

covars$gdp_percap <- log(covars$gdp_percap)

for (i in unique(covars$region)){
  if (i == 'HIC'){
    next
  }
  covars[ , paste0('region', i)] <- as.numeric(covars$region == i)
}


# Read in data
moddat <- merge(fies_subnat, 
                covars, 
                all.x=T, all.y=F) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame

preddat <- na.omit(preddat); summary(preddat)

exc <- c()
#exc <- c('mal_falciparum', 'mal_vivax', 'wasting', 'crop_prod')
exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland', 'mal_vivax')

# Set up Model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'Urban', 'Rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc)]

moddat <- moddat %>%
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))

## Mod+Sev
avg.moddat.mod <- list()
avg.preddat.mod <- list()
for(i in 1:100) {
  
  # Print iteration
  print(paste0("-------------------------------------------------------------------------| ", i, "/100 |"))
  
  if(i%%10 == 0 | i == 1) {
    # Tune parameters
    rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                              data = moddat,
                              mtryStart = max(1, floor(sqrt(length(vars)))), 
                              nodesizeTry = c(1:3),
                              ntreeTry = 500,
                              sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(4/5))),
                              trace = T,
                              doBest = T); rf.tune.mod$optimal
  }
  
  # Model
  rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                  data = moddat,
                  ntree = 1000, 
                  mtry = rf.tune.mod$optimal[[2]],
                  nodesize = rf.tune.mod$optimal[[1]],
                  do.trace = TRUE)
  
  # Get model predictions for each iter
  avg.moddat.mod[[paste0("fies.mod.pred_", i)]] <- inv.logit(as.numeric(predict(rf.mod, moddat)$predicted))
  avg.preddat.mod[[paste0("fies.mod.pred_", i)]] <- inv.logit(as.numeric(predict(rf.mod, preddat)$predicted))
  
}

# Average over all iters
avg.moddat.mod <- do.call(cbind, avg.moddat.mod)
avg.preddat.mod <- do.call(cbind, avg.preddat.mod)
moddat[[paste0("fies.mod.pred")]] <- apply(avg.moddat.mod, 1, mean)
preddat[[paste0("fies.mod.pred")]] <- apply(avg.preddat.mod, 1, mean)

## Sev
# Tune parameters
rf.tune.sev <- tune.rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                          data = moddat,
                          mtryStart = max(1, floor(sqrt(length(vars)))), 
                          nodesizeTry = c(1:3),
                          ntreeTry = 500,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(4/5))),
                          trace = T,
                          doBest = T); rf.tune.sev$optimal
# Model
rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 1000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = TRUE)

# Predictions
moddat[[paste0("fies.sev.pred")]] <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat[[paste0("fies.sev.pred")]] <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))


moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))

############################
# Visualize Results
############################

######################
# Save output for Poli
########################

sel <- preddat %>%
  select(ISO3, YEAR, GDLCODE, stunting, urban_perc, fies.mod.pred, population) %>%
  rename(fies.mod.pred = "fies.mod.pred") %>%
  merge(u5.population) %>%
  filter(YEAR %in% c(2020, 2025, 2030)) %>%
  mutate(u5pop.urban = urban_perc*u5pop,
         u5pop.rural = (1 - urban_perc)*u5pop,
         stunting.urban = stunting*u5pop.urban,
         stunting.rural = stunting*u5pop.rural,
         population.urban = population*urban_perc,
         population.rural = population*(1 - urban_perc),
         fies.urban = fies.mod.pred*population.urban,
         fies.rural = fies.mod.pred*population.rural) %>%
  select(-stunting, -urban_perc, -fies.mod.pred, -population, -u5pop) %>%
  gather(var, value, -ISO3, -YEAR, -GDLCODE) %>%
  mutate(GEO_AREA=ifelse(grepl('urban', var), 'urban', 'rural'),
         var=gsub('.rural|.urban', '', var),
         value = round(value)) %>%
  spread(var, value)

write.csv(sel, 'figures/fies.mod.results_randomforest.csv', row.names=F)

# #growth rate
# growth <- preddat %>%
#   filter(YEAR > 2010) %>%
#   group_by(YEAR) %>%
#   summarize(fies_total = sum(fies.mod.pred * (population), na.rm=T)) %>%
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
#   filter(YEAR %in% c(2020, 2025, 2030)) %>%
#   group_by(YEAR) %>%
#   summarize(fies_total = sum(fies.mod.pred * (population), na.rm=T)) %>%
#   mutate(abs = (fies_total-lag(fies_total))/5,
#          persec = abs/(31536000)) #31536000 second per 365 days/1 year

#####################
# Time Series
###############

#Get totals by year
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
       title="Number With Moderate or Severe Food Insecurity, By Continent, Stacked\n(Random Forest)") +
  theme_bw()
ggsave('figures/randomforest/Time.Mod.Stack_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='mod.total')) +
  geom_line(aes(x=YEAR, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Continent\n(Random Forest)") +
  theme_bw()
ggsave('figures/randomforest/Time.Mod.Lines_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Continent, Stacked\n(Random Forest)") +
  theme_bw()
ggsave('figures/randomforest/Time.Sev.Stack_randomforest.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) +
  geom_line(aes(x=YEAR, y=value, color=region), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Continent\n(Random Forest)") +
  theme_bw()
ggsave('figures/randomforest/Time.Sev.Lines_randomforest.png', width=7, height=5)


############################
# Make Map
##############################
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
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2 (Random Forest)',
       fill='') + 
  facet_grid(mod.YEAR ~ .)
ggsave('figures/randomforest/SSP2_Map_Moderate_randomforest.png', width=10, height=12)

ggplot(mapdat) +
  geom_sf(aes(fill=fies.sev.pred), color=NA) +
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0",
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) +
  geom_sf(data=countries, color='#000000', fill=NA) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(title='Rate of Severe Food Insecurity Under SSP2 (Random Forest)',
       fill='') +
  facet_grid(sev.YEAR ~ .)
ggsave('figures/randomforest/SSP2_Map_Severe_randomforest.png', width=10, height=12)


##################################
# Assess residuals
##################################
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  labs(title='Model Performance (Random Forest)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Mod+Sev Food Insecurity',
       y='Modeled Rate of Mod+Sev Food Insecurity')	
ggsave('figures/randomforest/SSP2_Mod_Residuals_randomforest.png', width=5, height=5)

# mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
# r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
# ggplot(moddat) + 
#   geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
#   labs(title='Model Performance (Random Forest)',
#        caption=paste0('Mean Absolute Error: ',  round(mae, 4),
#                       '\nR-squared: ', round(r2, 4)),
#        x='Observed Rates of Sev Food Insecurity',
#        y='Modeled Rate of Sev Food Insecurity')	
# ggsave('figures/randomforest/SSP2_Sev_Residuals_randomforest.png', width=5, height=5)


##################################
# Plot scaled covariates
#####################################


# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                ntree = 500)

# Variable Importance
png('figures/randomforest/SSP2_Mod_VIMP_randomforest.png', width = 1000, height = 500)
plot(vimp(rf.mod))
dev.off()

png('figures/randomforest/SSP2_Sev_VIMP_randomforest.png', width = 1000, height = 500)
plot(vimp(rf.sev))
dev.off()


# Variable Effect
png('figures/randomforest/SSP2_Mod_Coefs_randomforest.png', width = 1000, height = 800)
plot.variable.rfsrc(rf.mod, sorted = T)
dev.off()

png('figures/randomforest/SSP2_Sev_Coefs_randomforest.png', width = 1000, height = 800)
plot.variable.rfsrc(rf.sev, sorted = T)
dev.off()



############################
# IFAD call
############################
#graphs
# ggplot(totals %>% filter(var=='mod.total')) +
#   geom_line(aes(x=YEAR, y=value, color=region), size=1) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0), labels = scales::comma) +
#   labs(x='Year', y="Number Food Insecure",
#        title="Number With Moderate or Severe Food Insecurity, By Continent") + 
#   theme_bw() +
#   theme(legend.title=element_blank())
# ggsave('figures/IFAD/Time.Mod.Lines_randomforest.png', width=8, height=5)


#rural/urban
totals <- preddat %>%
  filter(YEAR > 2010) %>%
  #group_by(YEAR) %>%
  group_by(YEAR) %>%
  summarize(mod.rural=sum(fies.mod.pred * (population*rural_perc), na.rm=T),
            mod.urban=sum(fies.mod.pred * (population*urban_perc), na.rm=T)) %>%
  rename(Rural = "mod.rural", Urban = "mod.urban") %>%
  gather(var, value, -YEAR)

ggplot(totals) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/randomforest/Time.Mod.Lines.RurUrb_randomforest.png', width=8, height=5)


#map
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


