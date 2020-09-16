#######################################################
# Tune Random Forest model, Find the best model
#######################################################

# # read in data
# moddat <- merge(fies_subnat, 
#                 covars, 
#                 all.x=T, all.y=F) %>%
#   na.omit %>%
#   data.frame
# 
# preddat <- covars %>%
#   data.frame

moddat <- read.csv('data/moddat.csv')
preddat <- read.csv('data/preddat.csv')

exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland')

# set up model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            "fies.mod.pred", "fies.sev.pred", #only because cache is not updated
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc)]

moddat <- moddat %>%
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))


#take 25% of obersations as test sample
moddat_test <- sample_frac(moddat, size = 0.25, replace = F)
moddat_train <- setdiff(moddat, moddat_test)
moddat_test$i <- 1; moddat_train$i <- 0


## mod+sev
# tune parameters
rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat_train,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.mod$optimal

# model
rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat_train,
                ntree = 5000, 
                mtry = rf.tune.mod$optimal[[2]],
                nodesize = rf.tune.mod$optimal[[1]],
                do.trace = TRUE)


## sev
# tune parameters
rf.tune.sev <- tune.rfsrc(formula = as.formula(paste("fies.sev", 
                                                     paste(vars, collapse = "+"), sep= "~")),
                          data = moddat_train,
                          mtrystart = max(1, floor(sqrt(length(vars)))), 
                          nodesizetry = c(1:3),
                          ntreetry = 5000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.sev$optimal
# model
rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat_train,
                ntree = 5000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = TRUE)


# get predictions
moddat <- rbind(moddat_train, moddat_test)
moddat$fies.mod.pred <- inv.logit(predict(rf.mod, moddat)$predicted)
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))

moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))


# Validation wih test set
moddat_test <- moddat %>% filter(i == 1)

##################################
# assess residuals
##################################

setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
library(gridGraphics)
options(scipen=100)

mse <- mean(sqrt((moddat_test$fies.mod - moddat_test$fies.mod.pred)^2))
r2 <- cor(moddat_test$fies.mod, moddat_test$fies.mod.pred)
mod.res <- ggplot(moddat_test) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Squared Error: ',  round(mse, 4),
                      '\nR-Squared: ', round(r2, 4)),
       x='Observed Rates Of Moderat-to-Severe Food Insecurity',
       y='Modeled Rates Of Moderat-to-Severe Food Insecurity'); mod.res
#ggsave('model/mod.residuals_rf.png', width=5, height=5)

mse <- mean(sqrt((moddat_test$fies.sev - moddat_test$fies.sev.pred)^2))
r2 <- cor(moddat_test$fies.sev, moddat_test$fies.sev.pred)
sev.res <- ggplot(moddat_test) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Squared Error: ',  round(mse, 4),
                      '\nR-Squared: ', round(r2, 4)),
       x='Observed Rates Of Severe Food Insecurity',
       y='Modeled Rates Of Severe Food Insecurity')	; sev.res
#ggsave('model/sev.residuals_rf.png', width=5, height=5)

plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave('model/out-sample_rf.png', width=14, height=7)


