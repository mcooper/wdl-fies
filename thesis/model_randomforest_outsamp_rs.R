#######################################################
# Random Sample for Random Forest model
#######################################################

##----------------------------------------------------------------------------------------------
## use all countries in the sample

# do this out-of-sample validation 10 times
modval <- data.frame(index = 1:10, 
                     mae.mod = NA, mse.mod = NA, r2.mod = NA,
                     mae.sev = NA, mse.sev = NA, r2.sev = NA)


# read in data
moddat <- merge(fies_subnat,
                covars,
                all.x=T, all.y=F) %>%
  na.omit %>%
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


for(i in 1:10) {
  
  #take 25% of obersations as test sample
  moddat_test <- sample_frac(moddat, size = 0.25, replace = F)
  moddat_train <- setdiff(moddat, moddat_test)
  moddat_test$i <- 1; moddat_train$i <- 0
  
  
  ## mod+sev
  # tune parameters
  rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", 
                                                       paste(vars, collapse = "+"), sep= "~")),
                            data = moddat_train,
                            mtryStart = ncol(moddat_train)/2,
                            nodesizeTry = c(1:3),
                            ntree = 5000,
                            sampsize = min(nrow(moddat_train)*.632, max(150, nrow(moddat_train)^(3/4))),
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
                            mtryStart = ncol(moddat_train)/2,
                            nodesizeTry = c(1:3),
                            ntree = 5000,
                            sampsize = min(nrow(moddat_train)*.632, max(150, nrow(moddat_train)^(3/4))),
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
  moddat_test$fies.mod.pred <- inv.logit(as.numeric(predict(rf.mod, moddat_test)$predicted))
  moddat_test$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat_test)$predicted))
  
  
  # Validation with test set
  modval$mae.mod[modval$index == i] <- mean(abs(inv.logit(moddat_test$fies.mod) - moddat_test$fies.mod.pred))
  modval$mse.mod[modval$index == i] <- mean(sqrt((inv.logit(moddat_test$fies.mod) - moddat_test$fies.mod.pred)^2))
  modval$r2.mod[modval$index == i] <- cor(inv.logit(moddat_test$fies.mod), moddat_test$fies.mod.pred)
  
  modval$mae.sev[modval$index == i] <- mean(abs(inv.logit(moddat_test$fies.sev) - moddat_test$fies.sev.pred))
  modval$mse.sev[modval$index == i] <- mean(sqrt((inv.logit(moddat_test$fies.sev) - moddat_test$fies.sev.pred)^2))
  modval$r2.sev[modval$index == i] <- cor(inv.logit(moddat_test$fies.sev), moddat_test$fies.sev.pred)
  
}

# write.csv(modval, 'thesis/results/modval_rs.csv', row.names=F)
modval <- read.csv('thesis/results/modval_rs.csv')


# average all validation metrics
modval %>% 
  select(-index) %>%
  summarise_all(list(mean))



##----------------------------------------------------------------------------------------------
## exclude high income countries according to WB

income <- read.csv('thesis/income.csv')

# do this out-of-sample validation 10 times
modval <- data.frame(index = 1:10, 
                     mae.mod = NA, mse.mod = NA, r2.mod = NA,
                     mae.sev = NA, mse.sev = NA, r2.sev = NA)


# read in data
moddat <- merge(fies_subnat,
                covars,
                all.x=T, all.y=F) %>%
  filter(!ISO3 %in% income$ISO3) %>%
  na.omit %>%
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


for(i in 1:10) {
  
  #take 25% of obersations as test sample
  moddat_test <- sample_frac(moddat, size = 0.25, replace = F)
  moddat_train <- setdiff(moddat, moddat_test)
  moddat_test$i <- 1; moddat_train$i <- 0
  
  
  ## mod+sev
  # tune parameters
  rf.tune.mod <- tune.rfsrc(formula = as.formula(paste("fies.mod", 
                                                       paste(vars, collapse = "+"), sep= "~")),
                            data = moddat_train,
                            mtryStart = ncol(moddat_train)/2,
                            nodesizeTry = c(1:3),
                            ntree = 5000,
                            sampsize = min(nrow(moddat_train)*.632, max(150, nrow(moddat_train)^(3/4))),
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
                            mtryStart = ncol(moddat_train)/2,
                            nodesizeTry = c(1:3),
                            ntree = 5000,
                            sampsize = min(nrow(moddat_train)*.632, max(150, nrow(moddat_train)^(3/4))),
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
  moddat_test$fies.mod.pred <- inv.logit(as.numeric(predict(rf.mod, moddat_test)$predicted))
  moddat_test$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat_test)$predicted))
  
  
  # Validation with test set
  modval$mae.mod[modval$index == i] <- mean(abs(inv.logit(moddat_test$fies.mod) - moddat_test$fies.mod.pred))
  modval$mse.mod[modval$index == i] <- mean(sqrt((inv.logit(moddat_test$fies.mod) - moddat_test$fies.mod.pred)^2))
  modval$r2.mod[modval$index == i] <- cor(inv.logit(moddat_test$fies.mod), moddat_test$fies.mod.pred)
  
  modval$mae.sev[modval$index == i] <- mean(abs(inv.logit(moddat_test$fies.sev) - moddat_test$fies.sev.pred))
  modval$mse.sev[modval$index == i] <- mean(sqrt((inv.logit(moddat_test$fies.sev) - moddat_test$fies.sev.pred)^2))
  modval$r2.sev[modval$index == i] <- cor(inv.logit(moddat_test$fies.sev), moddat_test$fies.sev.pred)
  
}

# write.csv(modval, 'thesis/results/modval_wo_hic_rs.csv', row.names=F)
modval <- read.csv('thesis/results/modval_wo_hic_rs.csv')


# average all validation metrics
modval %>% 
  select(-index) %>%
  summarise_all(list(mean))



##################################
# assess residuals
##################################

moddat_test <- moddat_test %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))

#setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

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
ggsave('model/mod.residuals_rf.png', width=5, height=5)

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


