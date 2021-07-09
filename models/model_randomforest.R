######################################################
# use random forest with ssp covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

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

prm <- expand.grid(list(node=c(1:10, 12, 14, 16, 18, 20),
                        mtry=c(12:1),
                        depth=c(1:12, -1)))
prm$ix <- 1:nrow(prm)

prm$sev.rsq <- NA
prm$mod.rsq <- NA
prm$sev.mae <- NA
prm$mod.mae <- NA

#############################################
#Run model under cross validation
#at the country level
##########################################
rsq <- function(y, yhat){
  1 - (sum((y - yhat)^2))/sum((y - mean(yhat))^2)
}

for (i in sample(prm$ix[is.na(prm$sev.rsq)])){
  cat(round(sum(!is.na(prm$sev.rsq))/nrow(prm)*100, 2), 'percent done!\n')
  for (iso3 in unique(moddat$ISO3)){
    print(iso3)
    ix <- moddat$ISO3 != iso3
    
    mtry <- prm$mtry[i]
    node <- prm$node[i]
    if(prm$depth[i] < 0){
      depth <- prm$depth[i]
    } else{
      depth <- NULL
    }
    
    rf.mod <- rfsrc(formula = as.formula(paste("fies.mod.logit", 
                                               paste(vars, collapse = "+"), sep= "~")),
                    data = moddat[ix, ],
                    ntree = 1000, 
                    mtry = mtry,
                    nodesize = node,
                    depth = depth)
    
    rf.sev <- rfsrc(formula = as.formula(paste("fies.sev.logit", 
                                               paste(vars, collapse = "+"), sep= "~")),
                    data = moddat[ix, ],
                    ntree = 1000, 
                    mtry = mtry,
                    nodesize = node,
                    depth = depth)
      
    moddat$fies.mod.pred.cv[!ix] <- inv.logit(predict(rf.mod, moddat[!ix,])$predicted)
    moddat$fies.sev.pred.cv[!ix] <- inv.logit(predict(rf.sev, moddat[!ix,])$predicted)
    
  }
  prm$sev.rsq[i] <- cor(moddat$fies.sev, moddat$fies.sev.pred.cv)^2
  prm$mod.rsq[i] <- cor(moddat$fies.mod, moddat$fies.mod.pred.cv)^2
  prm$sev.mae[i] <- mean(abs(moddat$fies.sev.pred.cv - moddat$fies.sev))
  prm$mod.mae[i] <- mean(abs(moddat$fies.mod.pred.cv - moddat$fies.mod))
}

write.csv(prm, 'data/prm.csv', row.names=F)

mod.prm <- prm[which.max(prm$mod.rsq), ]
sev.prm <- prm[which.max(prm$sev.rsq), ]

system('~/telegram.sh "Done Running Random Forests"')

###################################################
# Re-run cross validation for best hyperparameters
#################################################
i <- mod.prm$ix
for (iso3 in unique(moddat$ISO3)){
  ix <- moddat$ISO3 != iso3
    
  mtry <- prm$mtry[i]
  node <- prm$node[i]
  if(prm$depth[i] < 0){
    depth <- prm$depth[i]
  } else{
    depth <- NULL
  }
  
  rf.mod <- rfsrc(formula = as.formula(paste("fies.mod.logit", 
                                             paste(vars, collapse = "+"), sep= "~")),
                  data = moddat[ix, ],
                  ntree = 1000, 
                  mtry = mtry,
                  nodesize = node,
                  depth = depth)
    
  moddat$fies.mod.pred.cv[!ix] <- inv.logit(predict(rf.mod, moddat[!ix,])$predicted)
}

i <- sev.prm$ix
for (iso3 in unique(moddat$ISO3)){
  ix <- moddat$ISO3 != iso3
  
  mtry <- prm$mtry[i]
  node <- prm$node[i]
  if(prm$depth[i] < 0){
    depth <- prm$depth[i]
  } else{
    depth <- NULL
  }
  
  rf.sev <- rfsrc(formula = as.formula(paste("fies.mod.logit", 
                                             paste(vars, collapse = "+"), sep= "~")),
                  data = moddat[ix, ],
                  ntree = 1000, 
                  mtry = mtry,
                  nodesize = node,
                  depth = depth)
    
  moddat$fies.sev.pred.cv[!ix] <- inv.logit(predict(rf.sev, moddat[!ix,])$predicted)
}


#########################################
# Run full models with best parameters
##########################################

rf.mod <- rfsrc(formula = as.formula(paste("fies.mod.logit", 
                                           paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = mod.prm$mtry,
                nodesize = mod.prm$node,
                depth = mod.prm$depth)

rf.sev <- rfsrc(formula = as.formula(paste("fies.sev.logit", 
                                           paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 5000, 
                mtry = sev.prm$mtry,
                nodesize = sev.prm$node,
                depth = sev.prm$depth)

moddat$fies.mod.logit <- NULL
moddat$fies.sev.logit <- NULL

# predictions
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))
moddat$fies.mod.pred <- inv.logit(as.numeric(predict(rf.mod, moddat)$predicted))
preddat$fies.mod.pred <- inv.logit(as.numeric(predict(rf.mod, preddat)$predicted))

write.csv(preddat, 'data/preddat.csv', row.names=F)
write.csv(moddat, 'data/moddat.csv', row.names=F)
save(list=c('rf.mod', 'rf.sev'), file='data/models.RData')
