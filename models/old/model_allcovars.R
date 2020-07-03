#######################################################
# Use LASSO regression with a large number of covariates
# to estimate current food insecurity, based on data for
# as close to 2018 as possible
#######################################################

# Read in data
moddat <- merge(fies_subnat %>%
                  rename(ISO3=iso3), 
								covar_now, 
								all.x=T, all.y=F) %>%
	na.omit %>%
	data.frame

preddat <- covar_now %>%
	filter(YEAR==2018) %>%
	data.frame

# Set up Model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur', 
																						'fies.sev.rur', 'fies.mod.urb', 
                                            'fies.sev.urb', 'fies.mod', 
																						'fies.sev', 'rural', 'urban', 'YEAR',
                                            'population', 'Rural', 'Urban',
                                            #Seems overfit -remove WGIs
                                            'control_corruption', 'regulatory_quality',
                                            'rule_of_law', 'government_effectiveness',
                                            'stability_noviolence', 'voice_accountability',
                                            'ducks', 'pigs', 'buffaloes', 'horses', 
                                            'mal_vivax', 'forest', 'bare', 'ndvi',
                                            'tavg', 'conflict_deaths_percap',
                                            'mean_annual_precip', 'shdi',
                                            'ruggedness'
                                            )]

x <- model.matrix(as.formula(paste0('fies.mod ~ ', paste0(vars, collapse=' + '))), data=moddat)

mod <- cv.glmnet(x, moddat$fies.mod, alpha=1)
tmp_coeffs <- coef(mod, s = "lambda.min")
df <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], 
                 estimate = tmp_coeffs@x,
								 stringsAsFactors=F)

# Get model predictions
preddat$fies.mod.pred <- df$estimate[df$term == '(Intercept)']
moddat$fies.mod.pred <- df$estimate[df$term == '(Intercept)']
for (i in 2:nrow(df)){
  preddat[ , paste0('v_', df$term[i])] <- preddat$fies.mod.pred + preddat[ , df$term[i]]*df$estimate[i]

	preddat$fies.mod.pred <- preddat$fies.mod.pred + preddat[ , df$term[i]]*df$estimate[i]
	moddat$fies.mod.pred <- moddat$fies.mod.pred + moddat[ , df$term[i]]*df$estimate[i]
}
preddat$fies.mod.pred[preddat$fies.mod.pred < 0] <- 0
preddat$fies.mod.pred[preddat$fies.mod.pred > 1] <- 1

#Get totals
totals <- preddat %>%
  group_by(YEAR) %>%
	summarize(total=prettyNum(sum(fies.mod.pred * population, na.rm=T), big.mark=',', scientific=F))

# Make Map
mapdat <- merge(gdl, preddat %>% select(GDLCODE, fies.mod.pred), all.x=T, all.y=F)
countries <- ne_countries(returnclass='sf')
ggplot(mapdat) + 
	geom_sf(aes(fill=fies.mod.pred), color=NA) + 
	scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
																	 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
	coord_sf(crs='+proj=robin') +  
	theme_void() + 
	theme(legend.position = 'bottom',
				plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate to Severe Food Insecurity in 2018',
			 subtitle=paste0(totals$total, ' People'),
			 fill='')
ggsave('figures/Pred2018_LASSO.png', width=7, height=5)

# Assess residuals
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
	geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
	labs(caption=paste0('Mean Absolute Error: ',  round(mae, 4), 
											'\nR-squared: ', round(r2, 4)),
			 x='Observed Rates of Food Insecurity',
			 y='Modeled Rate of Food Insecurity') 
ggsave('figures/Pred2018_Residuals.png', width=5, height=5)

# Plot scaled covariates
for (v in vars){
	if (v %in% df$term){
		df$scaled[df$term==v] <- df$estimate[df$term==v]*sd(moddat[ , v])
	} else{
		df <- bind_rows(df, data.frame(term=v, estimate=0, scaled=0))
	}
}

df$term <- factor(df$term, levels=df$term[order(df$scaled)], ordered=TRUE)

ggplot(df %>% filter(term != '(Intercept)')) + 
	  geom_bar(aes(x=term, y=scaled), stat='identity') + 
		coord_flip() + 
		labs(title='Change in Rate of Food Insecurity\nWith increase of 1 SD in Variable\nFor 2018 LASSO Regression Model', 
				 x="", 
				 y="") + 
		theme_minimal()

ggsave('figures/Pred2018_Coefs.png', width=5, height=5)







