

preddat <- read.csv('data/preddat.csv')
moddat <- read.csv('data/moddat.csv')

######################
# save output for poli
########################

sel <- preddat %>%
  select(ISO3, YEAR, GDLCODE, stunting, urban_perc, fies.mod.pred, population) %>%
  filter(YEAR %in% c(2020, 2025, 2030)) %>%
  merge(u5.population, all.x=T, all.y=F) %>%
  mutate(u5pop.urban = urban_perc*u5_frac*population,
         u5pop.rural = (1 - urban_perc)*u5_frac*population,
         stunting.urban = stunting*u5pop.urban,
         stunting.rural = stunting*u5pop.rural,
         population.urban = population*urban_perc,
         population.rural = population*(1 - urban_perc),
         fies.urban = fies.mod.pred*population.urban,
         fies.rural = fies.mod.pred*population.rural) %>%
  select(-stunting, -urban_perc, -fies.mod.pred, -population, -u5_frac) %>%
  gather(var, value, -ISO3, -YEAR, -GDLCODE) %>%
  mutate(geo_area=ifelse(grepl('urban', var), 'Urban', 'Rural'),
         var=gsub('.rural|.urban', '', var),
         value = round(value)) %>%
  spread(var, value)

write.csv(sel, 'figures/HC_2020-10-07.csv', row.names=F) #update date



##########################################
#### plot error and variable importance
##########################################

#setwd('~/wdl-fies/docs/img')
setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
library(gridGraphics)
options(scipen=100)

# change varaibles names
vars_full <- c("Stunting", "Wasting", "Mean Years of Schooling", "Topographic Ruggedness",
              "GDP Per Capita", "Gini Coefficient", "Malaria Mortality Rate", "Mean Annual Precipitation",
              "Poverty Headcount Index", "Mean Temperature", "Urban Percentage", "Water Scarcity") #has to be in the same order as "vars"

rf.mod$yvar.names <- c("Moderate-to-Severe Food Insecurity")
rf.mod$xvar.names <- vars_full
rf.sev$yvar.names <- c("Severe Food Insecurity")
rf.sev$xvar.names <- vars_full

# hv.mod <- holdout.vimp(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                 data = moddat,
#                 ntree = 5000, 
#                 mtry = rf.tune.mod$optimal[[2]],
#                 nodesize = rf.tune.mod$optimal[[1]],
#                 verbose = TRUE)

# error rate and variable importance
plot(vimp(rf.mod), plots.one.page = F, sorted = T, verbose = T)
grid.echo(); mod.error <- grid.grab() #save plot individually
grid.echo(); mod.vimp <- grid.grab()

plot(vimp(rf.sev), plots.one.page = F, sorted = T, verbose = T)
grid.echo(); sev.error <- grid.grab() #save plot individually
grid.echo(); sev.vimp <- grid.grab()

plot_grid(mod.error, sev.error, labels='AUTO', scale = 0.8)
ggsave('model/error_rf.png', width=14, height=7)

plot_grid(mod.vimp, sev.vimp, labels='AUTO', scale = 0.8)
ggsave('model/vimp_rf.png', width=17, height=7)



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

