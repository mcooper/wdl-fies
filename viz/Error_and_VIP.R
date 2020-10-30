# setwd('~/wdl-fies');library(ProjectTemplate);load.project()
#setwd('~/wdl-fies/docs/img')
setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
library(gridGraphics)
options(scipen=100)

####################################
# Variable Importance
###################################

vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'fies.mod.pred', 'fies.sev.pred',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region')]

# change varaibles names
vars_full <- c("Stunting", "Wasting", "Mean Years of Schooling", "Topographic Ruggedness",
              "GDP Per Capita", "Gini Coefficient", "Malaria Mortality Rate", "Mean Annual Precipitation",
              "Poverty Headcount Index", "Mean Temperature", "Urban Percentage", "Water Scarcity") #has to be in the same order as "vars"

v.mod <- vimp(rf.mod, importance='permute')
v.sev <- vimp(rf.sev, importance='permute')

df <- data.frame(var=c(names(v.mod$importance), names(v.sev$importance)),
                 val=c(v.mod$importance, v.sev$importance),
                 mod=rep(c('Moderate', 'Severe'), each=12),
                 lab=rep(vars_full, 2))

df$lab <- factor(df$lab, levels=vars_full[order(v.sev$importance)])

convertToOutcomeScale <- function(x){
  #Determine how an error of x on a logit transformed scale
  #affects the mean absolute error after being transformed back to [0,1]
  mean(abs(moddat$fies.mod - inv.logit(logit(moddat$fies.mod) + x)))
}

df$val <- sapply(df$val, convertToOutcomeScale)

ggplot(df) + 
  geom_bar(aes(x=lab, y=val), stat='identity', fill='#0F1290') +
  coord_flip() + 
  facet_grid(. ~ mod) + 
  theme_bw() + 
  labs(x='', y='Increase in Model Error When Variable is Permutated')
ggsave('VIMP.pdf', width=7, height=3.75)

##################################
# assess residuals
##################################
mae.mod <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
mae.sev <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))

r2.mod <- cor(moddat$fies.mod, moddat$fies.mod.pred)
r2.sev <- cor(moddat$fies.sev, moddat$fies.sev.pred)


mod.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Squared Error: ',  round(mse, 4),
                      '\nR-Squared: ', round(r2, 4)),
       x='Observed Rates Of Moderat-to-Severe Food Insecurity',
       y='Modeled Rates Of Moderat-to-Severe Food Insecurity'); mod.res
#ggsave('model/mod.residuals_rf.png', width=5, height=5)

mse <- mean(sqrt((moddat$fies.sev - moddat$fies.sev.pred)^2))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
sev.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Squared Error: ',  round(mse, 4),
                      '\nR-Squared: ', round(r2, 4)),
       x='Observed Rates Of Severe Food Insecurity',
       y='Modeled Rates Of Severe Food Insecurity')	; sev.res
#ggsave('model/sev.residuals_rf.png', width=5, height=5)

plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave('model/in-sample_rf.png', width=14, height=7)

