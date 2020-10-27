# setwd('~/wdl-fies');library(ProjectTemplate);load.project()
#setwd('~/wdl-fies/docs/img')
setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
library(gridGraphics)
options(scipen=100)

vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'fies.mod.pred', 'fies.sev.pred',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region')]

# change varaibles names
vars_full <- c("Stunting", "Wasting", "Mean Years of Schooling", "Topographic Ruggedness",
              "GDP Per Capita", "Gini Coefficient", "Malaria Mortality Rate", "Mean Annual Precipitation",
              "Poverty Headcount Index", "Mean Temperature", "Urban Percentage", "Water Scarcity") #has to be in the same order as "vars"


hv.mod <- holdout.vimp(formula = as.formula(paste("fies.mod", 
                                                  paste(vars, collapse = "+"), sep= "~")),
                 data = moddat,
                 ntree = 5000, 
                 mtry = rf.tune.mod$optimal[[2]],
                 nodesize = rf.tune.mod$optimal[[1]],
                 verbose = TRUE)

hv.sev <- holdout.vimp(formula = as.formula(paste("fies.sev", 
                                                  paste(vars, collapse = "+"), sep= "~")),
                 data = moddat,
                 ntree = 5000, 
                 mtry = rf.tune.sev$optimal[[2]],
                 nodesize = rf.tune.sev$optimal[[1]],
                 verbose = TRUE)

df <- data.frame(var=c(names(hv.mod), names(hv.sev)),
                 val=c(hv.mod, hv.sev),
                 mod=rep(c('Moderate', 'Severe'), each=12),
                 lab=rep(vars_full, 2))

df$lab <- factor(df$lab, levels=vars_full[order(hv.mod)])

ggplot(df) + 
  geom_bar(aes(x=lab, y=val), stat='identity', fill='#0F1290') +
  coord_flip() + 
  facet_grid(. ~ mod) + 
  theme_bw()

# variable effect
# png('model/mod.coefs_rf.png', width = 1000, height = 800, units="px")
# plot.variable.rfsrc(rf.mod, sorted = T)
# dev.off()
# 
# png('model/sev.coefs_rf.png', width = 1000, height = 800, units="px")
# plot.variable.rfsrc(rf.sev, sorted = T)
# dev.off()
# 
# plot.variable.rfsrc(rf.mod, sorted = T)
# grid.echo()
# mod.coef <- grid.grab()
# 
# plot.variable.rfsrc(rf.sev, sorted = T)
# grid.echo()
# sev.coef <- grid.grab()
# 
# plot_grid(plot.variable.rfsrc(rf.mod, sorted = T), plot.variable.rfsrc(rf.mod, sorted = T), align='h',labels='AUTO', scale = 0.8)
# ggsave('model/coef_rf.png', width=18, height=8)



##################################
# assess residuals
##################################
mse <- mean(sqrt((moddat$fies.mod - moddat$fies.mod.pred)^2))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
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

