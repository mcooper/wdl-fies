#######################################################
# Use LASSO regression with SSP covariates
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

###################################
# Model moderate food insecurity
##################################

x <- model.matrix(as.formula(paste0('fies.mod ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, moddat$fies.mod, alpha=1)
mod$lambda.min

tmp_coeffs <- coef(mod, s = "lambda.min")
mdf <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], estimate =tmp_coeffs@x,
                  stringsAsFactors=F)

# Get model predictions
preddat$fies.mod.pred <- mdf$estimate[mdf$term == '(Intercept)']
moddat$fies.mod.pred <- mdf$estimate[mdf$term == '(Intercept)']
for (i in 2:nrow(mdf)){
  preddat$fies.mod.pred <- preddat$fies.mod.pred + preddat[ , mdf$term[i]]*mdf$estimate[i]
  moddat$fies.mod.pred <- moddat$fies.mod.pred + moddat[ , mdf$term[i]]*mdf$estimate[i]
}
preddat$fies.mod.pred <- inv.logit(preddat$fies.mod.pred)
moddat$fies.mod.pred <- inv.logit(moddat$fies.mod.pred)


###################################
# Model severe food insecurity
###################################

x <- model.matrix(as.formula(paste0('fies.sev ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, moddat$fies.sev, alpha=1)
mod$lambda.min

tmp_coeffs <- coef(mod, s = "lambda.min")
sdf <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], estimate = tmp_coeffs@x,
                  stringsAsFactors=F)

# Get model predictions
preddat$fies.sev.pred <- sdf$estimate[sdf$term == '(Intercept)']
moddat$fies.sev.pred <- sdf$estimate[sdf$term == '(Intercept)']
for (i in 2:nrow(sdf)){
  preddat$fies.sev.pred <- preddat$fies.sev.pred + preddat[ , sdf$term[i]]*sdf$estimate[i]
  moddat$fies.sev.pred <- moddat$fies.sev.pred + moddat[ , sdf$term[i]]*sdf$estimate[i]
}
preddat$fies.sev.pred <- inv.logit(preddat$fies.sev.pred)
moddat$fies.sev.pred <- inv.logit(moddat$fies.sev.pred)

moddat$fies.mod <- inv.logit(moddat$fies.mod)
moddat$fies.sev <- inv.logit(moddat$fies.sev)

##################################
# Model specific outputs/plots
##################################

# assess residuals
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (lasso)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Mod+Sev Food Insecurity',
       y='Modeled Rate of Mod+Sev Food Insecurity')	
ggsave('figures/lasso/Residuals.Mod_lasso.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (lasso)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Sev Food Insecurity',
       y='Modeled Rate of Sev Food Insecurity')	
ggsave('figures/lasso/Residuals.Sev_lasso.png', width=5, height=5)



# plot scaled covariates
for (v in vars){
  if (v %in% mdf$term){
    mdf$scaled[mdf$term==v] <- mdf$estimate[mdf$term==v]*sd(moddat[ , v])
  } else{
    mdf <- bind_rows(mdf, data.frame(term=v, estimate=0, scaled=0))
  }
}
mdf$term <- factor(mdf$term, levels=mdf$term[order(mdf$scaled)], ordered=TRUE)

ggplot(mdf %>% filter(term != '(Intercept)')) + 
  geom_bar(aes(x=term, y=scaled), stat='identity') + 
  coord_flip() + 
  labs(title='Change in Rate of Mod+Sev Food Insecurity\nWith increase of 1 SD in Var (lasso)',
       x="", y="") + 
  scale_y_continuous(breaks = seq(from = -1, to = 1, by = 0.25)) +
  theme_minimal()
ggsave('figures/lasso/Coef.Mod_lasso.png', width=6, height=5)


for (v in vars){
  if (v %in% sdf$term){
    sdf$scaled[sdf$term==v] <- sdf$estimate[sdf$term==v]*sd(moddat[ , v])
  } else{
    sdf <- bind_rows(sdf, data.frame(term=v, estimate=0, scaled=0))
  }
}
sdf$term <- factor(sdf$term, levels=sdf$term[order(sdf$scaled)], ordered=TRUE)

ggplot(sdf %>% filter(term != '(Intercept)')) + 
  geom_bar(aes(x=term, y=scaled), stat='identity') + 
  coord_flip() + 
  labs(title='Change in Rate of Sev Food Insecurity\nWith increase of 1 SD in Var (lasso)',
       x="", y="") + 
  theme_minimal()
ggsave('figures/lasso/Coef.Sev_lasso.png', width=6, height=5)


m <- "lasso"

