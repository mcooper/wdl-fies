######################################################
# use random forest with ssp covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

set.seed(100)

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
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))

prm <- expand.grid(list(node=c(1:7),
                        mtry=c(6,5,4,3),
                        depth=c(1:5, -1)))
prm$ix <- 1:nrow(prm)


#Run model under k-fold cross validation
iso3s <- unique(moddat$ISO3)
samp <- sample(1:10, length(iso3s), replace=T)
for (i in 1:nrow(prm)){
  for (s in 1:10){
    ix <- moddat$ISO3 %in% iso3s[samp != s]

    mtry <- prm$mtry[i]
    node <- prm$node[i]
    if(prm$depth[i] < 0){
      depth <- prm$depth[i]
    } else{
      depth <- nULL
    }

    rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                    data = moddat[ix, ],
                    ntree = 1000, 
                    mtry = mtry,
                    nodesize = node,
                    depth = depth,
                    do.trace = TRUE)
    
    rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                    data = moddat[ix, ],
                    ntree = 1000, 
                    mtry = mtry,
                    nodesize = node,
                    depth = depth,
                    do.trace = TRUE)
      
    moddat$fies.mod.pred[!ix] <- inv.logit(predict(rf.mod, moddat[!ix,])$predicted)
    moddat$fies.sev.pred[!ix] <- inv.logit(predict(rf.sev, moddat[!ix,])$predicted)

    prm$sev.rsq <- cor(moddat$fies.sev.pred, moddat$fies.sev)^2
    prm$mod.rsq <- cor(moddat$fies.mod.pred, moddat$fies.mod)^2
  }
}


# model
rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 1000, 
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
                          mtryStart = ncol(moddat)/2, 
                          nodesizeTry = c(1:3),
                          ntree = 1000,
                          sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(3/4))),
                          trace = T,
                          dobest = T); rf.tune.sev$optimal
# model
rf.sev <- rfsrc(formula = as.formula(paste("fies.sev", paste(vars, collapse = "+"), sep= "~")),
                data = moddat,
                ntree = 1000, 
                mtry = rf.tune.sev$optimal[[2]],
                nodesize = rf.tune.sev$optimal[[1]],
                do.trace = TRUE)

# predictions
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))

#Re-transform true outcomes
moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))

write.csv(preddat, 'data/preddat.csv', row.names=F)
write.csv(moddat, 'data/moddat.csv', row.names=F)
save(list=c('rf.mod', 'rf.sev', 'rf.tune.mod', 'rf.tune.sev'), file='data/models.RData')
