#######################################################
# Tune Random Forest model, Find the best model
#######################################################

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


# Play around with model variables
exc <- list(
  c(), #1
  
  c('crops_prod', 'forest', 'builtup', 'livestock', #2
    'pasture', 'crops_prod', 'cropland',
    'mal_vivax', 'wasting', 'mal_falciparum'),
  
  c('crops_prod', 'builtup', 'livestock', #3
    'pasture', 'crops_prod', 'cropland',
    'mal_vivax', 'wasting'),
  
  c('crops_prod', 'builtup', 'livestock', #4
    'pasture', 'crops_prod', 'cropland',
    'wasting'),
  
  c('crops_prod', 'forest', 'builtup', 'livestock', #5
    'pasture', 'crops_prod', 'cropland',
    'mal_vivax', 'wasting', 'mal_falciparum',
    'school_mean', 'mean_annual_precip',
    'tavg', 'wci_index', 'ruggedness', 'urban_perc'),
  
  c('crops_prod', 'forest', 'builtup', 'livestock', #6
    'pasture', 'crops_prod', 'cropland',
    'mal_vivax', 'wasting', 'mal_falciparum',
    'wci_index'),
  
  c('crops_prod', 'mal_vivax', 'wasting', #7
    'mal_falciparum', 'wci_index'),
  
  c('crops_prod', 'forest', 'builtup', 'livestock', #8
    'pasture', 'cropland', 'mal_vivax'),
  
  c('crops_prod', 'forest', 'builtup', 'livestock', #9
    'pasture', 'cropland', 'mal_vivax', 'wasting')
)

tsplit <- c(0.25) #split training and test, tsplit = % of test sample
ntree <- c(500) #number of trees
exc <- exc[[8]] #go with 8, no IAM var and one malaria


# Read in data
moddat <- merge(fies_subnat, 
                covars, 
                all.x=T, all.y=F) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame




# Set up Model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'Urban', 'Rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc)]

moddat <- moddat %>% mutate(fies.mod = logit(fies.mod))


# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                conservative = "low",
#                                ntree = 500)


#take x% of obersations as test sample
moddat_test <- sample_frac(moddat, size = tsplit[1], replace = F)
moddat_train <- setdiff(moddat, moddat_test)
moddat_test$i <- 1; moddat_train$i <- 0

rf.tune <- tune.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                      data = moddat_train,
                      mtryStart = max(1, floor(sqrt(length(vars)))), 
                      nodesizeTry = c(1:15),
                      ntreeTry = 500,
                      sampsize = min(nrow(moddat)*.632, max(150, nrow(moddat)^(4/5))),
                      trace = T,
                      doBest = T); rf.tune$optimal


# plot.new()
# text(x=0.1, y=0.8, paste0("exc: ", e), pos=4, offset=0)
# text(x=0.1, y=0.7, paste0("tsplit: ", tsplit), pos=4, offset=0)
# text(x=0.1, y=0.6, paste0("ntree: ", ntree), pos=4, offset=0)
# text(x=0.1, y=0.5, paste0("mtry: ", rf.tune$optimal[[2]]), pos=4, offset=0)
# text(x=0.1, y=0.4, paste0("nodesize: ", rf.tune$optimal[[1]]), pos=4, offset=0)


# Model
rf.mod <- rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
                data = moddat_train,
                #x_test = mxtest, y_test = mytest,
                ntree = ntree[1], 
                mtry = rf.tune$optimal[[2]],
                nodesize = rf.tune$optimal[[1]],
                do.trace = TRUE)

# vimp.rfsrc(rf.mod)$importance

# with(rfcv(trainx = mxtrain, trainy = mytrain, cv.fold = 3),
#      plot(n.var, error.cv, log = "x", type = "o", lwd = 1))

moddat <- rbind(moddat_train, moddat_test)
moddat <- moddat %>% mutate(fies.mod = inv.logit(fies.mod))

# Get model predictions

whatna <- subset(preddat, is.na(preddat$gini)) #we got NA's in preddat

moddat[[paste0("fies.mod.pred_median")]] <- inv.logit(predict(rf.mod, moddat)$predicted)
preddat <- na.omit(preddat); summary(preddat)
preddat[[paste0("fies.mod.pred_median")]] <- inv.logit(predict(rf.mod, preddat)$predicted)


# Validation
moddat_test <- moddat %>% filter(i == 1)

# MEA and R^2
moddat_test$abs <- abs(moddat_test$fies.mod - moddat_test$fies.mod.pred_median)

mse <- mean(sqrt((moddat_test$fies.mod - moddat_test$fies.mod.pred_median)^2))
r2 <- cor(moddat_test$fies.mod, moddat_test$fies.mod.pred_median)
print(ggplot(moddat_test) + 
        geom_point(aes(x=fies.mod, y=fies.mod.pred_median)) +
        geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
        labs(title= paste0("Performance: ", 1-tsplit[1], " training, ", tsplit[1], " validation"),
             caption=paste0('Mean Squared Error: ',  round(mse, 4),
                            '\nR-squared: ', round(r2, 4)),
             x='Observed Rates of Mod+Sev Food Insecurity',
             y='Modeled Rate of Mod+Sev Food Insecurity'))
ggsave('figures/randomforest/SSP2_Mod_OOS_Error_randomforest.png', width=5, height=5)


# Get totals by year and region graphs
totals <- preddat %>%
  filter(YEAR > 2010) %>%
  #group_by(YEAR) %>%
  group_by(YEAR, region) %>%
  summarize(mod.total_median=sum(fies.mod.pred_median * (population), na.rm=T)) %>%
  gather(var, value, -YEAR, -region)

print(ggplot(totals %>% filter(var=='mod.total_median')) +
        geom_line(aes(x=YEAR, y=value, color=region), size=1) +
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        labs(x='Year', y="Number Food Insecure",
             title="Number With Moderate or Severe Food Insecurity, By Continent") +
        theme_bw())


# Maps
mapdat <- merge(gdl,
                preddat %>%
                  filter(YEAR %in% c(2020, 2025, 2030)) %>%
                  select(GDLCODE, YEAR, fies.mod.pred_median),
                all.x=T, all.y=F) %>%
  merge(totals %>%
          filter(YEAR %in% c(2020, 2025, 2030)) %>%
          group_by(YEAR, var) %>%
          summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
          spread(var, value) %>%
          mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total_median,
                                   '\nIn Moderate or Severe Food Insecurity')))

countries <- ne_countries(returnclass='sf')
print(ggplot(mapdat) +
        geom_sf(aes(fill=fies.mod.pred_median), color=NA) +
        scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0",
                                       "#fce08a", "#faae61", "#f36c44", "#a01c44")) +
        geom_sf(data=countries, color='#000000', fill=NA) +
        coord_sf(crs='+proj=robin') +
        theme_void() +
        theme(legend.position = 'bottom',
              plot.title = element_text(hjust = 0.5)) +
        labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2',
             fill='') +
        facet_grid(mod.YEAR ~ .))







# pdf("models/model_randomforest_tune.pdf", paper = "a4")
# for(e in 1:length(exc)) {
#   
# }
# dev.off()







#-------------------------------------------------------------------------------------------------------

# # Play around with model variables
# exc <- list(
#   # c(),
#   
#   # c('crops_prod', 'forest', 'builtup', 'livestock',
#   #   'pasture', 'crops_prod', 'cropland',
#   #   'mal_vivax', 'wasting', 'mal_falciparum'),
#   
#   c('crops_prod', 'builtup', 'livestock', #1
#     'pasture', 'crops_prod', 'cropland',
#     'mal_vivax', 'wasting'),
#   
#   c('crops_prod', 'builtup', 'livestock', #2
#     'pasture', 'crops_prod', 'cropland',
#     'wasting')
# )
# 
# tsplit <- c(0.1, 0.25, 0.5)
# ntree <- c(200, 300, 400)
# 
# 
# i <- 1
# 
# #pdf("models/model_randomforest_tune.pdf", paper = "a4")
# for(e in 1:length(exc)) {
#   for(ts in 1:length(tsplit)) {
#     for(nt in 1:length(ntree)) {
#       for(mt in 1:length(mtry)) {
#         
#         plot.new()
#         text(x=0.1, y=0.9, paste0(i, ". Setting of RF model:"), pos=4, offset=0, font=2)
#         text(x=0.1, y=0.8, paste0("exc: ", e), pos=4, offset=0)
#         text(x=0.1, y=0.7, paste0("tsplit: ", tsplit[ts]), pos=4, offset=0)
#         text(x=0.1, y=0.6, paste0("ntree: ", ntree[nt]), pos=4, offset=0)
#         text(x=0.1, y=0.5, paste0("mtry: ", mtry[mt]), pos=4, offset=0)
#         
#         # Read in data
#         moddat <- merge(fies_subnat, 
#                         covars, 
#                         all.x=T, all.y=F) %>%
#           na.omit %>%
#           data.frame
#         
#         preddat <- covars %>%
#           data.frame
#         
#         
#         # Set up Model
#         vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
#                                                     'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
#                                                     'Urban', 'Rural', 'fies.sev', 'fies.mod',
#                                                     'population', 'YEAR', 'rural_perc', 'region',
#                                                     names(moddat)[grepl('region', names(moddat))],
#                                                     exc[[e]])]
#         
# 
#         moddat <- moddat %>% mutate(fies.mod = logit(fies.mod))
#         
#         #take 50% of obersations as test sample
#         moddat_test <- sample_frac(moddat, size = tsplit[ts], replace = F)
#         moddat_train <- setdiff(moddat, moddat_test)
#         moddat_test$i <- 1
#         moddat_train$i <- 0
#         
#         
#         rf.mod <- randomForest(as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")), 
#                                data = moddat_train,
#                                ntree = ntree[nt], 
#                                mtry = as.data.frame(
#                                  tuneRF(
#                                    x = as.matrix(moddat_train %>% select(vars)), 
#                                    y = as.matrix(moddat_train %>% select(fies.mod)))
#                                )$mtry[tRF$OOBError == min(tRF$OOBError)],
#                                importance = TRUE,
#                                do.trace = TRUE)
#         
#         
#         moddat <- rbind(moddat_train, moddat_test)
#         moddat <- moddat %>% mutate(fies.mod = inv.logit(fies.mod))
#         
#         # Get model predictions
#         preddat[[paste0("fies.mod.pred_median")]] <- inv.logit(predict(rf.mod, preddat))
#         moddat[[paste0("fies.mod.pred_median")]] <- inv.logit(predict(rf.mod, moddat))
#         
#         
#         # Validation
#         moddat_test <- moddat %>% filter(i == 1)
#         
#         # MEA and R^2
#         mae <- mean(abs(moddat_test$fies.mod - moddat_test$fies.mod.pred_median))
#         r2 <- cor(moddat_test$fies.mod, moddat_test$fies.mod.pred_median)
#         print(ggplot(moddat_test) + 
#           geom_point(aes(x=fies.mod, y=fies.mod.pred_median)) +
#           geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
#           labs(title= paste0("Performance: ", 1-tsplit[ts], " training, ", tsplit[ts], " validation"),
#                caption=paste0('Mean Absolute Error: ',  round(mae, 4),
#                               '\nR-squared: ', round(r2, 4)),
#                x='Observed Rates of Mod+Sev Food Insecurity',
#                y='Modeled Rate of Mod+Sev Food Insecurity'))	
#         #varImpPlot(rf.mod, main = "RF Mod+Sev: Variables")
#         
#         
# # Get totals by year and region graphs
# totals <- preddat %>%
#   filter(YEAR > 2010) %>%
#   #group_by(YEAR) %>%
#   group_by(YEAR, region) %>%
#   summarize(mod.total_median=sum(fies.mod.pred_median * (population), na.rm=T)) %>%
#   gather(var, value, -YEAR, -region)
# 
# print(ggplot(totals %>% filter(var=='mod.total_median')) +
#   geom_line(aes(x=YEAR, y=value, color=region), size=1) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   labs(x='Year', y="Number Food Insecure",
#        title="Number With Moderate or Severe Food Insecurity, By Continent") +
#   theme_bw())
# 
# 
# # Maps
# mapdat <- merge(gdl %>%
#                   select(-region),
#                 preddat %>%
#                   filter(YEAR %in% c(2020, 2025, 2030)) %>%
#                   select(GDLCODE, YEAR, fies.mod.pred_median),
#                 all.x=T, all.y=F) %>%
#   merge(totals %>%
#           filter(YEAR %in% c(2020, 2025, 2030)) %>%
#           group_by(YEAR, var) %>%
#           summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
#           spread(var, value) %>%
#           mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total_median,
#                                    '\nIn Moderate or Severe Food Insecurity')))
# 
# countries <- ne_countries(returnclass='sf')
# print(ggplot(mapdat) +
#   geom_sf(aes(fill=fies.mod.pred_median), color=NA) +
#   scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0",
#                                  "#fce08a", "#faae61", "#f36c44", "#a01c44")) +
#   geom_sf(data=countries, color='#000000', fill=NA) +
#   coord_sf(crs='+proj=robin') +
#   theme_void() +
#   theme(legend.position = 'bottom',
#         plot.title = element_text(hjust = 0.5)) +
#   labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2',
#        fill='') +
#   facet_grid(mod.YEAR ~ .))

#         i <- i+1
#       }
#     }
#   }
# }
# 
# dev.off()