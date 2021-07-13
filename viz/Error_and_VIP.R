setwd('~/wdl-fies');library(ProjectTemplate);load.project()
setwd('~/wdl-fies/docs/img')
#setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
library(gridGraphics)
options(scipen=100)

rsq <- function(y, yhat){
  1 - (sum((y - yhat)^2))/sum((y - mean(yhat))^2)
}

####################################
# Variable Importance
###################################

vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'fies.mod.pred', 'fies.sev.pred',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            'fies.mod.logit', 'fies.sev.logit', 
                                            'fies.mod.pred.cv', 'fies.sev.pred.cv')]

# change varaibles names
vars_full <- c("Stunting", "Wasting", "Mean Years of Schooling", "Topographic Ruggedness",
              "GDP Per Capita", "Gini Coefficient", "Malaria Mortality Rate",
              "Total Annual Precipitation", 
              "Poverty Headcount Index", "Mean Temperature", 
              "Total Annual Precipitation\n(Previous Year)", "Mean Temperature\n(Previous Year)",
              "Urban Percentage", "Water Scarcity") #has to be in the same order as "vars"

v.mod <- vimp(rf.mod, importance='permute')
v.sev <- vimp(rf.sev, importance='permute')

#####################################
# Plot Variable importance
##################################

df <- data.frame(var=c(names(v.mod$importance), names(v.sev$importance)),
                 val=c(v.mod$importance, v.sev$importance),
                 mod=rep(c('Moderate', 'Severe'), each=14),
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

################################
# Plot Error Over Time
#################################

erdf <- data.frame(var=c(v.mod$err.rate, v.sev$err.rate),
                 trees=c(1:5000, 1:5000),
                 mod=rep(c('Moderate', 'Severe'), each=5000)) %>%
  na.omit

ggplot(erdf) + 
  geom_line(aes(x=trees, y=var)) + 
  facet_wrap(. ~ mod, scales='free_y') + 
  theme_bw() + 
  labs(x='Number of Trees', y='Error Rate')
ggsave('Error_Over_Training.pdf', width=7, height=3.75)

##################################
# assess residuals
##################################
mae.mod <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
mae.sev <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))

r2.mod <- rsq(moddat$fies.mod, moddat$fies.mod.pred)
r2.sev <- rsq(moddat$fies.sev, moddat$fies.sev.pred)

mod.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.mod, 4),
                      '\nR-Squared: ', round(r2.mod, 4)),
       x='Observed Moderate Food Insecurity',
       y='Modeled Moderate Food Insecurity'); mod.res

sev.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.sev, 4),
                      '\nR-Squared: ', round(r2.sev, 4)),
       x='Observed Severe Food Insecurity',
       y='Modeled Severe Food Insecurity')	; sev.res

grd <- plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave(grd, filename='in-sample_rf.pdf', width=7, height=3.75)

#########################################
# Out of Sample Residuals
#########################################
mae.mod <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred.cv))
mae.sev <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred.cv))

r2.mod <- rsq(moddat$fies.mod, moddat$fies.mod.pred.cv)
r2.sev <- rsq(moddat$fies.sev, moddat$fies.sev.pred.cv)

mod.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred.cv), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.mod, 4),
                      '\nR-Squared: ', round(r2.mod, 4)),
       x='Observed Moderate Food Insecurity',
       y='Modeled Moderate Food Insecurity'); mod.res

sev.res <- ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred.cv), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.sev, 4),
                      '\nR-Squared: ', round(r2.sev, 4)),
       x='Observed Severe Food Insecurity',
       y='Modeled Severe Food Insecurity')	; sev.res

grd <- plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave(grd, filename='out-sample_rf.pdf', width=7, height=3.75)


