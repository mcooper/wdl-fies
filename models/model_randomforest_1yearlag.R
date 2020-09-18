#######################################################
# use random forest with ssp covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

# setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

#Add 1-year lags as features
prev_year <- covars %>%
  mutate(YEAR = YEAR - 1)

ix <- !names(prev_year) %in% c('GDLCODE', 'YEAR', 'ISO3')
names(prev_year)[ix] <- paste0('prev_', names(prev_year)[ix])

covars <- merge(covars, prev_year)

# read in data
moddat <- merge(fies_subnat, 
                covars, 
                all.x=T, all.y=F) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame

exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland')

# set up model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc, paste0('prev_', exc))]

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
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.mod$optimal

# model
rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.mod$optimal[[2]],
                nodesize = rf.tune.mod$optimal[[1]],
                do.trace = TRUE)

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
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.sev$optimal
# model
rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = TRUE)

# predictions
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))

moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))

write.csv(preddat, 'data/preddat.csv', row.names=F)
write.csv(moddat, 'data/moddat.csv', row.names=F)

######################
# save output for poli
########################

sel <- preddat %>%
  select(ISO3, YEAR, GDLCODE, stunting, urban_perc, fies.mod.pred, population) %>%
  filter(YEAR %in% c(2020, 2025, 2030)) %>%
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
  gather(var, value, -ISO3, -YEAR, -GDLCODE) %>%
  mutate(geo_area=ifelse(grepl('urban', var), 'Urban', 'Rural'),
         var=gsub('.rural|.urban', '', var),
         value = round(value)) %>%
  spread(var, value)

write.csv(sel, 'figures/fies.mod.results_randomforest.csv', row.names=F)



##################################
#### plot scaled covariates
########################################


# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                ntree = 500)

setwd('~/wdl-fies/docs/img')

# variable importance
png('mod_vimp.png', width = 1000, height = 500, units="px")
plot(vimp(rf.mod))
dev.off()

png('sev_vimp.png', width = 1000, height = 500, units="px")
plot(vimp(rf.sev))
dev.off()

# variable effect
png('mod_coefs.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.mod, sorted = T)
dev.off()

png('sev_coefs.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.sev, sorted = T)
dev.off()

