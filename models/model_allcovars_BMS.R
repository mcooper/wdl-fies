#######################################################
# Use BMA regression with a large number of covariates
# to estimate current food insecurity, based on data for
# as close to 2020 as possible
#######################################################

# if 1, interaction terms included
action <- 0 #keep "0" for now


# Read in data
moddat <- merge(fies_subnat, 
								covar_now, 
								all.x=T, all.y=F) %>%
	na.omit %>%
	data.frame

preddat <- covar_now %>%
	filter(year==2020) %>%
	data.frame

# Set up Model, try BMS
vars <- names(moddat)[!names(moddat) %in% c('iso3', 'GDLCODE', 'fies.mod.rur', 'fies.sev.rur', 
																						'fies.mod.urb', 'fies.sev.urb', 'Urban', 'Rural', 'fies.mod',
																						'fies.sev', 'rural', 'urban', 'year')]
m_sel <- moddat %>% select(fies.mod, vars)

if(action == 1) {
  #generate interaction terms
  int <- m_sel[,2:ncol(m_sel)]
  f <- as.formula(paste0("~(", paste0(colnames(int), collapse="+"), ")","^2"))
  
  x <- data.frame(model.matrix(f, int[1,]))
  for(i in 2:nrow(int)) {
    x[i,] <- data.frame(model.matrix(f, int[i,]))
    if((i %% 100) == 0) {print(i)}
  }
  
  x <- x[, 2:ncol(x)]
  m_sel <- cbind(m_sel %>% select(fies.mod), x)
  colnames(m_sel) <- gsub("[.]", "#", colnames(m_sel))
}

#install.packages("BMS")
library(BMS)

if(action == 1) {
  mod <- bms(m_sel, burn = 1000, iter = 2000, mprior = "random", mcmc = "bd.int"); summary(mod)
  } else {mod <- bms(m_sel, burn = 100000, iter = 200000, mprior = "random", mcmc = "bd"); summary(mod)}

coef(mod, std.coefs = T, order.by.pip = T, include.constant = F)

what <- fullmodel.ssq(mod)


p_sel <- preddat %>% select(year, GDLCODE, vars)
p_sel[is.na(p_sel)] <- 0 #set all NA's equal to 0
summary(p_sel)

if(action == 1) {
  #generate interaction terms
  int <- p_sel[,3:ncol(p_sel)]
  f <- as.formula(paste0("~(", paste0(colnames(int), collapse="+"), ")","^2"))

  x <- data.frame(model.matrix(f, int[1,]))
  for(i in 2:nrow(int)) {
    x[i,] <- data.frame(model.matrix(f, int[i,]))
    if((i %% 100) == 0) {print(i)}
  }

  p_sel <- x[, 2:ncol(x)]
  colnames(p_sel) <- gsub("[.]", "#", colnames(p_sel))
} else{p_sel <- p_sel[,3:ncol(p_sel)]}

cool <- data.frame(preddat %>% select(year, GDLCODE), 
                   fies.mod.pred = predict(mod, newdata = p_sel, exact = FALSE, topmodels = NULL))

summary(cool)
cool$fies.mod.pred[cool$fies.mod.pred < 0] <- 0
cool$fies.mod.pred[cool$fies.mod.pred > 1] <- 1
summary(cool)


#Get totals by year
totals <- cool %>%
	merge(ssp_future %>%
					filter(year==2020) %>%
				  select(GDLCODE, urban, rural)) %>%
	group_by(year) %>%
	summarize(total=prettyNum(sum(fies.mod.pred * (rural + urban), na.rm=T), big.mark=',', scientific=F))


# Make Map
mapdat <- merge(gdl, cool %>% select(GDLCODE, fies.mod.pred), all.x=T, all.y=F)
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
  labs(title='Rate of Moderate to Severe Food Insecurity in 2020 (BMS)',
			 subtitle=paste0(totals$total, ' People'),
			 fill='')

if(action == 1) {
  ggsave('figures/Pred2020_BMS_interaction.png', width=7, height=5)
} else {ggsave('figures/Pred2020_BMS.png', width=7, height=5)}






