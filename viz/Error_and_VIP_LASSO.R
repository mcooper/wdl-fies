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
# Variable Coefficients
###################################

tmp_coeffs <- coef(lasso.mod, s = 'lambda.min')
sevdf <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
                    estimate = tmp_coeffs@x) %>%
  mutate(mod='Severe')

tmp_coeffs <- coef(lasso.sev, s = 'lambda.min')
moddf <- data.frame(term = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
                    estimate = tmp_coeffs@x) %>%
  mutate(mod='Moderate')

df <- bind_rows(sevdf, moddf) %>%
  filter(term != '(Intercept)')

for (t in unique(df$term)){
  df$estimate[df$term == t] <- df$estimate[df$term == t]*sd(moddat.lasso[ , t])
}

vars <- names(moddat.lasso)[!names(moddat.lasso) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
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

#####################################
# Plot Variable importance
##################################

labdf <- data.frame(term=vars, lab=vars_full)

m <- merge(df, labdf, all=T)

m <- m %>%
  complete(lab, mod)

m$estimate[is.na(m$estimate)] <- 0

levels <- m %>%
  filter(mod == 'Severe') %>%
  arrange(estimate) %>%
  pull(lab)

m$lab <- factor(m$lab, levels=levels)

ggplot(m) + 
  geom_bar(aes(x=lab, y=estimate), stat='identity', fill='#0F1290') +
  coord_flip() + 
  facet_grid(. ~ mod) + 
  theme_bw() + 
  labs(x='', y='Change in Outcome for 1 St.Dev. Increase in Variable')
ggsave('VIMP_Lasso.pdf', width=7, height=3.75)

##################################
# assess residuals
##################################
mae.mod <- mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred))
mae.sev <- mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred))

r2.mod <- rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred)
r2.sev <- rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred)

mod.res <- ggplot(moddat.lasso) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.mod, 4),
                      '\nR-Squared: ', round(r2.mod, 4)),
       x='Observed Moderate Food Insecurity',
       y='Modeled Moderate Food Insecurity'); mod.res

sev.res <- ggplot(moddat.lasso) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.sev, 4),
                      '\nR-Squared: ', round(r2.sev, 4)),
       x='Observed Severe Food Insecurity',
       y='Modeled Severe Food Insecurity')	; sev.res

grd <- plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave(grd, filename='in-sample_lasso.pdf', width=7, height=3.75)

#########################################
# Out of Sample Residuals
#########################################
mae.mod <- mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred.cv))
mae.sev <- mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred.cv))

r2.mod <- rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred.cv)
r2.sev <- rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred.cv)

mod.res <- ggplot(moddat.lasso) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred.cv), alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.mod, 4),
                      '\nR-Squared: ', round(r2.mod, 4)),
       x='Observed Moderate Food Insecurity',
       y='Modeled Moderate Food Insecurity'); mod.res

sev.res <- ggplot(moddat.lasso) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred.cv), alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  xlim(0, 1) + ylim(0, 1) +
  labs(caption=paste0('Mean Average Error: ',  round(mae.sev, 4),
                      '\nR-Squared: ', round(r2.sev, 4)),
       x='Observed Severe Food Insecurity',
       y='Modeled Severe Food Insecurity')	; sev.res

grd <- plot_grid(mod.res, sev.res, align='h',labels='AUTO')
ggsave(grd, filename='out-sample_lasso.pdf', width=7, height=3.75)


