#######################################################
# Use Random Forest regression with SSP covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

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
                                            exc)]

moddat <- moddat %>%
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))


###########################
# Tune, Model and Predict
###########################

rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.mod$optimal

rf.tune.sev <- tune.rfsrc(formula = as.formula(paste("fies.sev", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.sev$optimal



rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.mod$optimal[[2]],
                nodesize = rf.tune.mod$optimal[[1]],
                do.trace = TRUE)

rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = TRUE)



moddat$fies.mod.pred <- inv.logit(predict(rf.mod, moddat)$predicted)
preddat$fies.mod.pred <- inv.logit(predict(rf.mod, preddat)$predicted)

moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))


moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))


##################################
# Model specific outputs/plots
##################################

# assess residuals
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (randomforest)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-Squared: ', round(r2, 4)),
       x='Observed Rates of Mod+Sev Food Insecurity',
       y='Modeled Rate of Mod+Sev Food Insecurity')	
ggsave('figures/randomforest/Residuals.Mod_randomforest.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (randomforest)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Sev Food Insecurity',
       y='Modeled Rate of Sev Food Insecurity')	
ggsave('figures/randomforest/Residuals.Sev_randomforest.png', width=5, height=5)



# plot scaled covariates
# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                ntree = 500)


# variable importance
png('figures/randomforest/VIMP.Mod_randomforest.png', width = 1000, height = 500, units="px")
plot(vimp(rf.mod))
dev.off()

png('figures/randomforest/VIMP.Sev_randomforest.png', width = 1000, height = 500, units="px")
plot(vimp(rf.sev))
dev.off()


# variable effect
png('figures/randomforest/Coefs.Mod_randomforest.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.mod, sorted = T)
dev.off()

png('figures/randomforest/Coefs.Sev_randomforest.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.sev, sorted = T)
dev.off()


m <- "randomforest"

