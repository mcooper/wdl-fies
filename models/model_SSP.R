#######################################################
# Use LASSO regression with SSP covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

#Just to fix Siberia
covars$wci_index[covars$GDLCODE=='RUSr108'] <- 1

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
                                            'mal_vivax', 'stunting')]

####################################
# Model moderate food insecurity
##################################
x <- model.matrix(as.formula(paste0('fies.mod ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, moddat$fies.mod, alpha=1)
mod$lambda.min

tmp_coeffs <- coef(mod, s = "lambda.min")
df <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], estimate = tmp_coeffs@x,
								                  stringsAsFactors=F)

# Get model predictions
preddat$fies.mod.pred <- df$estimate[df$term == '(Intercept)']
moddat$fies.mod.pred <- df$estimate[df$term == '(Intercept)']
for (i in 2:nrow(df)){
	preddat$fies.mod.pred <- preddat$fies.mod.pred + preddat[ , df$term[i]]*df$estimate[i]
	moddat$fies.mod.pred <- moddat$fies.mod.pred + moddat[ , df$term[i]]*df$estimate[i]
}
preddat$fies.mod.pred[preddat$fies.mod.pred < 0] <- 0
preddat$fies.mod.pred[preddat$fies.mod.pred > 1] <- 1

###################################
# Model severe food insecurity
########################################
x <- model.matrix(as.formula(paste0('fies.sev ~ ', paste0(vars, collapse=' + '))), 
                  data=moddat)

mod <- cv.glmnet(x, moddat$fies.sev, alpha=1)
mod$lambda.min

tmp_coeffs <- coef(mod, s = "lambda.min")
df <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], estimate = tmp_coeffs@x,
								                  stringsAsFactors=F)

# Get model predictions
preddat$fies.sev.pred <- df$estimate[df$term == '(Intercept)']
moddat$fies.sev.pred <- df$estimate[df$term == '(Intercept)']
for (i in 2:nrow(df)){
	preddat$fies.sev.pred <- preddat$fies.sev.pred + preddat[ , df$term[i]]*df$estimate[i]
	moddat$fies.sev.pred <- moddat$fies.sev.pred + moddat[ , df$term[i]]*df$estimate[i]
}
preddat$fies.sev.pred[preddat$fies.sev.pred < 0] <- 0
preddat$fies.sev.pred[preddat$fies.sev.pred > 1] <- 1



##########################################
# Visualize Results
############################

#Get totals by year
totals <- preddat %>%
  filter(YEAR > 2010) %>%
	group_by(YEAR) %>%
	summarize(mod.total=sum(fies.mod.pred * (population), na.rm=T),
            sev.total=sum(fies.sev.pred * (population), na.rm=T)) %>%
  gather(var, value, -YEAR) %>%
  mutate(value_clean = prettyNum(value, big.mark=',', scientific=F))

ggplot(totals) + 
  geom_line(aes(x=YEAR, y=value, color=var))

############################
# Make Map
##############################
mapdat <- merge(gdl, preddat %>% select(GDLCODE, YEAR, fies.mod.pred, fies.sev.pred), all.x=T, all.y=F) %>%
	merge(totals %>%
          filter(YEAR %in% c(2020, 2025, 2030)) %>%
          select(-value) %>%
          spread(var, value_clean)) %>%
	mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total, '\nIn Moderate or Severe Food Insecurity'),
         sev.YEAR = paste0(YEAR, ':\n', sev.total, '\nIn Severe Food Insecurity'))

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
			x='Observed Rates of Food Insecurity',
			y='Modeled Rate of Food Insecurity')	
ggsave('figures/SSP2_Residuals.png', width=5, height=5)

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
		labs(title='Change in Rate of Food Insecurity\nWith increase of 1 SD in Var\nFor SSP2 LASSO Regression Model',
				 x="", y="") + 
		theme_minimal()

ggsave('figures/SSP2_Coefs.png', width=5, height=5)







