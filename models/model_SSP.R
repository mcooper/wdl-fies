#######################################################
# Use LASSO regression with SSP covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

#Just to fix Siberia
covars$wci_index[covars$GDLCODE=='RUSr108'] <- 1

covars$gdp_percap <- log(covars$gdp_percap)

for (i in unique(covars$region)){
  if (i == 'HIC'){
    next
  }
  covars[ , paste0('region', i)] <- as.numeric(covars$region == i)
}


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
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',                                             'Urban', 'Rural', 'fies.mod', 'fies.sev', 
                                            'population', 'YEAR', 'rural_perc',
                                            'crops_prod', 'forest', 'builtup', 'livestock',
                                            'pasture', 'crops_prod', 'cropland', 
                                            'mal_vivax', 'stunting', 'region',
                                            names(moddat)[grepl('region', names(moddat))])]
####################################
# Model moderate food insecurity
##################################
x <- model.matrix(as.formula(paste0('fies.mod ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, logit(moddat$fies.mod), alpha=1)
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
########################################
x <- model.matrix(as.formula(paste0('fies.sev ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, logit(moddat$fies.sev), alpha=1)
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


##########################################
# Visualize Results
############################

#####################
# Time Series
###############

#Get totals by year
totals <- preddat %>%
  filter(YEAR > 2010) %>%
	group_by(YEAR, region) %>%
	summarize(mod.total=sum(fies.mod.pred * (population), na.rm=T),
            sev.total=sum(fies.sev.pred * (population), na.rm=T)) %>%
  gather(var, value, -YEAR, -region)

ggplot(totals %>% filter(var=='mod.total')) + 
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Continent, Stacked") + 
  theme_bw()
ggsave('figures/Time.Mod.Stack.png', width=7, height=5)

ggplot(totals %>% filter(var=='mod.total')) + 
  geom_line(aes(x=YEAR, y=value, color=region), size=1) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Continent") + 
  theme_bw()
ggsave('figures/Time.Mod.Lines.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) + 
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Continent, Stacked") + 
  theme_bw()
ggsave('figures/Time.Sev.Stack.png', width=7, height=5)

ggplot(totals %>% filter(var=='sev.total')) + 
  geom_line(aes(x=YEAR, y=value, color=region), size=1) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Continent") + 
  theme_bw()
ggsave('figures/Time.Sev.Lines.png', width=7, height=5)


############################
# Make Map
##############################
mapdat <- merge(gdl %>%
                  select(-region), 
                preddat %>% 
                  filter(YEAR %in% c(2020, 2025, 2030)) %>%
                  select(GDLCODE, YEAR, fies.mod.pred, fies.sev.pred), 
                all.x=T, all.y=F) %>%
	merge(totals %>%
          filter(YEAR %in% c(2020, 2025, 2030)) %>%
          group_by(YEAR, var) %>%
          summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
          spread(var, value) %>%
         	mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total, 
                           '\nIn Moderate or Severe Food Insecurity'),
                 sev.YEAR = paste0(YEAR, ':\n', sev.total, 
                           '\nIn Severe Food Insecurity')))

countries <- ne_countries(returnclass='sf')
ggplot(mapdat) + 
	  geom_sf(aes(fill=fies.mod.pred), color=NA) + 
		scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
																	 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
	coord_sf(crs='+proj=robin') + 
	theme_void() + 
	theme(legend.position = 'bottom',
				plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate to Severe Food Insecurity Under SSP2',
			 fill='') + 
	facet_wrap(mod.YEAR ~ .)
ggsave('figures/SSP2_LASSO_Moderate.png', width=20, height=8)

ggplot(mapdat) + 
	  geom_sf(aes(fill=fies.sev.pred), color=NA) + 
		scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
																	 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
	coord_sf(crs='+proj=robin') + 
	theme_void() + 
	theme(legend.position = 'bottom',
				plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Severe Food Insecurity Under SSP2',
			 fill='') + 
	facet_wrap(sev.YEAR ~ .)
ggsave('figures/SSP2_LASSO_Severe.png', width=20, height=8)

##################################
# Assess residuals
##################################
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
	geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
	labs(caption=paste0('Mean Absolute Error: ',  round(mae, 4),
											'\nR-squared: ', round(r2, 4)),
			x='Observed Rates of Mod+Sev Food Insecurity',
			y='Modeled Rate of Mod+Sev Food Insecurity')	
ggsave('figures/SSP2_Mod_Residuals.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) + 
	geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
	labs(caption=paste0('Mean Absolute Error: ',  round(mae, 4),
											'\nR-squared: ', round(r2, 4)),
			x='Observed Rates of Sev Food Insecurity',
			y='Modeled Rate of Sev Food Insecurity')	
ggsave('figures/SSP2_Sev_Residuals.png', width=5, height=5)

##################################
# Plot scaled covariates
#####################################
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
		labs(title='Change in Rate of Mod+Sev Food Insecurity\nWith increase of 1 SD in Var\nFor SSP2 LASSO Regression Model',
				 x="", y="") + 
		theme_minimal()

ggsave('figures/SSP2_Mod_Coefs.png', width=5, height=5)



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
		labs(title='Change in Rate of Sev Food Insecurity\nWith increase of 1 SD in Var\nFor SSP2 LASSO Regression Model',
				 x="", y="") + 
		theme_minimal()

ggsave('figures/SSP2_Sev_Coefs.png', width=5, height=5)






