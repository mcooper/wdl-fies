setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

# read in data
moddat <- merge(fies_subnat,
                covars,
                all.x=T, all.y=F) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame

# set up model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region')]

moddat <- moddat %>%
  mutate(fies.mod.logit = logit(fies.mod),
         fies.sev.logit = logit(fies.sev))

mm <- model.matrix(as.formula(paste("fies.mod.logit", 
                                           paste(vars, collapse = "+"), sep= "~")),
                data = moddat)
pm <- model.matrix(as.formula(paste("YEAR", 
                                           paste(vars, collapse = "+"), sep= "~")),
                data = preddat)

#############################################
#Run model under cross validation
#at the country level
##########################################
rsq <- function(y, yhat){
  1 - (sum((y - yhat)^2))/sum((y - mean(yhat))^2)
}

for (iso3 in unique(moddat$ISO3)){
  print(iso3)
  ix <- moddat$ISO3 != iso3
  
  lasso.mod <- cv.glmnet(mm[ix, ], moddat$fies.mod.logit[ix], alpha=1)
  lasso.sev <- cv.glmnet(mm[ix, ], moddat$fies.sev.logit[ix], alpha=1)

  moddat$fies.mod.pred.cv[!ix] <- inv.logit(predict(lasso.mod, mm[!ix,])[, 1])
  moddat$fies.sev.pred.cv[!ix] <- inv.logit(predict(lasso.sev, mm[!ix,])[, 1])
}

sev.rsq <- cor(moddat$fies.sev, moddat$fies.sev.pred.cv)^2
mod.rsq <- cor(moddat$fies.mod, moddat$fies.mod.pred.cv)^2
sev.mae <- mean(abs(moddat$fies.sev.pred.cv - moddat$fies.sev))
mod.mae <- mean(abs(moddat$fies.mod.pred.cv - moddat$fies.mod))

##################################
# Re-run model with all records
##################################
lasso.mod <- cv.glmnet(mm, moddat$fies.mod.logit, alpha=1)
lasso.sev <- cv.glmnet(mm, moddat$fies.sev.logit, alpha=1)

moddat$fies.mod.logit <- NULL
moddat$fies.sev.logit <- NULL

# predictions
moddat$fies.sev.pred <- inv.logit(predict(lasso.sev, mm)[, 1])
preddat$fies.sev.pred <- inv.logit(predict(lasso.sev, pm)[, 1])
moddat$fies.mod.pred <- inv.logit(predict(lasso.mod, mm)[, 1])
preddat$fies.mod.pred <- inv.logit(predict(lasso.mod, pm)[, 1])

write.csv(preddat, 'data/preddat_lasso.csv', row.names=F)
write.csv(moddat, 'data/moddat_lasso.csv', row.names=F)
save(list=c('lasso.mod', 'lasso.sev'), file='data/models_lasso.RData')
