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
  mutate(fies.mod.logit = logit(fies.mod),
         fies.sev.logit = logit(fies.sev))

prm <- expand.grid(list(node=c(1:10, 12, 14, 16, 20, 25, 100),
                        mtry=c(6:1),
                        depth=c(1:7, -1)))

prm$ix <- 1:nrow(prm)

#Run model under 10-fold cross validation
#at the country level
iso3s <- unique(moddat$ISO3)
samp <- sample(1:10, length(iso3s), replace=T)
for (i in 1:nrow(prm)){
  cat(round(i/nrow(prm)*100, 2), 'percent done!\n')
  for (s in 1:10){
    ix <- moddat$ISO3 %in% iso3s[samp != s]
    
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
      
    moddat$fies.mod.pred[!ix] <- inv.logit(predict(rf.mod, moddat[!ix,])$predicted)
    moddat$fies.sev.pred[!ix] <- inv.logit(predict(rf.sev, moddat[!ix,])$predicted)
    
    prm$sev.rsq[i] <- cor(moddat$fies.sev.pred, moddat$fies.sev)^2
    prm$mod.rsq[i] <- cor(moddat$fies.mod.pred, moddat$fies.mod)^2
  }
}

system('~/telegram.sh "Done Running Random Forests"')

# predictions
moddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, moddat)$predicted))
preddat$fies.sev.pred <- inv.logit(as.numeric(predict(rf.sev, preddat)$predicted))

write.csv(preddat, 'data/preddat.csv', row.names=F)
write.csv(moddat, 'data/moddat.csv', row.names=F)
save(list=c('rf.mod', 'rf.sev', 'rf.tune.mod', 'rf.tune.sev'), file='data/models.RData')
