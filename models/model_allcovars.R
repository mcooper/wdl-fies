#######################################################
# Use LASSO regression with a large number of covariates
# to estimate current food insecurity, based on data for
# as close to 2020 as possible
#######################################################

# Read in data
moddat <- merge(fies_subnat, 
								covar_ext, 
								all.x=T, all.y=F) %>%
	na.omit %>%
	data.frame

preddat <- covar_ext %>%
	filter(year==2020) %>%
	data.frame

# Set up Model
vars <- names(moddat)[!names(moddat) %in% c('iso3', 'GDLCODE', 'fies.mod.rur', 'fies.sev.rur', 
																						'fies.mod.urb', 'fies.sev.urb', 'Urban', 'Rural', 'fies.mod', 
																						'fies.sev', 'rural', 'urban', 'year')]

x <- model.matrix(as.formula(paste0('fies.mod ~ ', paste0(vars, collapse=' + '))), data=moddat)

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

#Get totals by year
totals <- preddat %>%
	merge(ssp_future %>%
					filter(year==2020) %>%
				  select(GDLCODE, urban, rural)) %>%
	group_by(year) %>%
	summarize(total=prettyNum(sum(fies.mod.pred * (rural + urban), na.rm=T), big.mark=',', scientific=F))

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
  labs(title='Rate of Moderate to Severe Food Insecurity in 2020',
			 subtitle=paste0(totals$total, ' People'),
			 fill='')
ggsave('figures/Pred2020_LASSO.png', width=7, height=5)

# Assess residuals
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
ggplot(moddat) + 
	geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
	labs(caption=paste0('MAE: ',  mae)) 
ggsave('figures/Pred2020_Residuals.png', width=5, height=5)

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
		labs(title='Change in Rate of Food Insecurity\nWith increase of 1 SD in Variable\nFor 2020 LASSO Regression Model') + 
		theme_minimal()

ggsave('figures/Pred2020_Coefs.png', width=5, height=5)







