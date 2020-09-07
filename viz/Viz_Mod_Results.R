# setwd('~/wdl-fies'); library(ProjectTemplate); load.project(list(data_loading=T))

#Save outputs to wdl-fies-tex for manuscript
setwd('~/wdl-fies-tex/img')
library(cowplot)
options(scipen=100)

############################
# visualize results
############################

########################
# Make Map
##########################

mapdat <- merge(gdl,
                preddat %>%
                  filter(YEAR %in% c(2010, 2020, 2030)) %>%
                  select(YEAR, GDLCODE, fies.mod.pred, fies.sev.pred) %>%
                  gather(outcome, value, -YEAR, -GDLCODE) %>%
                  mutate(FIES=ifelse(grepl('sev', outcome), 'Severe\n', 'Moderate-to-Severe\n')))

ggplot(mapdat) +
  geom_sf(aes(fill=value), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44"), 
                       breaks=seq(0, 1, by=0.2),
                       limits=c(0, max(mapdat$value))) + 
  geom_sf(data=cty, color='#000000', fill=NA, size=0.15) + 
  coord_sf(crs='+proj=robin', expand=FALSE) + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.spacing.x = unit(-0.5, "in")) + 
  labs(fill='') + 
  facet_grid(YEAR ~ FIES, switch='y')
ggsave('FullMap.pdf', width=7, dpi=750)
system('pdfcrop FullMap.pdf FullMap.pdf')


###########################
# Setup Reference Map
############################
region_cols <- c("South Asia" = "#e41a1c",
                 "Sub-Saharan Africa" = "#377eb8",
                 "Europe & Central Asia" = "#4daf4a",
                 "Middle East & North Africa" = "#f781bf",
                 "Latin America & Caribbean" = "#ff7f00",
                 "East Asia & Pacific" = "#a65628",
                 "North America" = "#984ea3")

cty <- ne_countries(returnclass='sf') %>%
  filter(region_wb != 'Antarctica')

reg <- ggplot(cty) + 
  geom_sf(aes(fill=region_wb), color=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  scale_fill_manual(values=region_cols) + 
  guides(fill=guide_legend(ncol=2)) + 
  labs(fill='')

#####################
# time series
###############

#get totals by YEAR
totals <- preddat %>%
  mutate(region = ifelse(region == 'Middle Esat & North Africa',
                         'Middle East & North Africa', region)) %>%
  filter(YEAR >= 2010) %>%
  group_by(YEAR, region) %>%
  summarize(mod.total=sum(fies.mod.pred * (population), na.rm=t),
            sev.total=sum(fies.sev.pred * (population), na.rm=t)) %>%
  gather(var, value, -YEAR, -region) %>%
  mutate(var = ifelse(grepl('mod', var), "Moderate-to-Severe", "Severe")) %>%
  group_by(region, var) %>%
  mutate(value = rollapply(value, width=3, FUN=mean, align='center', partial=TRUE))


stack <- ggplot(totals) +
  geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
  scale_fill_manual(values=region_cols) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05), add=0), 
                     labels=function(x){prettyNum(x, big.mark=',')}) +
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(0, 0.5, 0, 0), 'cm'),
        panel.spacing=unit(2, 'lines')) +
  labs(y="", title="", x='') +
  guides(fill=FALSE) + 
  facet_grid(. ~ var)

lines <- ggplot(totals) +
  geom_line(aes(x=YEAR, y=value, color=region), size=1) +
  scale_color_manual(values=region_cols) + 
  theme_bw() + 
  theme(plot.margin=unit(c(-1, 0.5, 0, 0), 'lines'),
        strip.text.x = element_blank(),
        panel.spacing=unit(2, 'lines')) + 
  scale_x_continuous(expand=c(0,0), labels=seq(2010, 2030, by=5)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05), add=0), 
                     labels=function(x){prettyNum(x, big.mark=',')}) +  
  labs(x='', y="",
       title="") +
  guides(color=FALSE) + 
  facet_grid(. ~ var)

plot_grid(plot_grid(stack, lines, align='v', nrow=2, labels='AUTO'), 
          reg, ncol=1, rel_heights=c(5, 1))
ggsave('TimeSeries.pdf', width=7, height=7)

##################################
# assess residuals
##################################
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  labs(title='model performance (random forest)',
       caption=paste0('mean absolute error: ',  round(mae, 4),
                      '\nr-squared: ', round(r2, 4)),
       x='observed rates of mod+sev food insecurity',
       y='modeled rate of mod+sev food insecurity')	
ggsave('mod_residuals.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.sev, y=fies.sev.pred)) + 
  labs(title='model performance (random forest)',
       caption=paste0('mean absolute error: ',  round(mae, 4),
                      '\nr-squared: ', round(r2, 4)),
       x='observed rates of sev food insecurity',
       y='modeled rate of sev food insecurity')	
ggsave('sev_residuals.png', width=5, height=5)


##################################
# plot scaled covariates
#####################################


# rf.var.sel <- var.select.rfsrc(formula = as.formula(paste("fies.mod", paste(vars, collapse = "+"), sep= "~")),
#                                data = moddat,
#                                method = "md",
#                                ntree = 500)

# variable importance
png('mod_vimp.png', width = 1000, height = 500, units="px")
plot(vimp(rf.mod))
dev.off()

png('sev_vimp.png', width = 1000, height = 500, units="px")
plot(vimp(rf.sev))
dev.off()


# variable effect
png('mod_coefs.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.mod, sorted = t)
dev.off()

png('sev_coefs.png', width = 1000, height = 800, units="px")
plot.variable.rfsrc(rf.sev, sorted = t)
dev.off()



############################
# ifad call
############################
#graphs
# ggplot(totals %>% filter(var=='mod.total')) +
#   geom_line(aes(x=YEAR, y=value, color=region), size=1) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0), labels = scales::comma) +
#   labs(x='YEAR', y="number food insecure",
#        title="number with moderate or severe food insecurity, by continent") + 
#   theme_bw() +
#   theme(legend.title=element_blank())
# ggsave('figures/ifad/time.mod.lines.png', width=8, height=5)


#rural/urban
totals <- preddat %>%
  filter(YEAR > 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.rural=sum(fies.mod.pred * (population*rural_perc), na.rm=T),
            mod.urban=sum(fies.mod.pred * (population*urban_perc), na.rm=T),
            sev.rural=sum(fies.sev.pred * (population*rural_perc), na.rm=T),
            sev.urban=sum(fies.sev.pred * (population*urban_perc), na.rm=T)) %>%
  gather(var, value, -YEAR) %>%
  mutate(out = ifelse(grepl('mod', var), 'Mod', 'Sev'),
         var = ifelse(grepl('rural', var), 'Rural', 'Urban'))

ggplot(totals %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('Time.Mod.Lines.RurUrb.png', width=8, height=5)


ggplot(totals %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('Time.Sev.Lines.RurUrb.png', width=8, height=5)

#Proportion Urban or Rural
prop <- preddat %>%
  mutate(pop.rural=population*rural_perc,
         pop.urban=population*urban_perc,
         mod.rural=fies.mod.pred * pop.rural,
         mod.urban=fies.mod.pred * pop.urban,
         sev.rural=fies.sev.pred * pop.rural,
         sev.urban=fies.sev.pred * pop.urban) %>%
  group_by(YEAR) %>%
  summarize_at(vars(matches('urban$|rural$')), sum) %>%
  mutate(sev.rural=sev.rural/pop.rural,
         sev.urban=sev.urban/pop.urban,
         mod.rural=mod.rural/pop.rural,
         mod.urban=mod.urban/pop.urban) %>%
  select(-matches('pop')) %>%
  gather(var, value, -YEAR) %>%
  mutate(out = ifelse(grepl('mod', var), 'Mod', 'Sev'),
         var = ifelse(grepl('rural', var), 'Rural', 'Urban'))

ggplot(prop %>% filter(out == 'Mod')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Proportion of Urban and Rural People With Moderate or Severe Food Insecurity") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('Time.Mod.Lines.Prop.RurUrb.png', width=8, height=5)


ggplot(prop %>% filter(out == 'Sev')) +
  geom_line(aes(x=YEAR, y=value, color=var), size=1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Severe Food Insecurity, By Rural/Urban") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('Time.Sev.Lines.Prop.RurUrb.png', width=8, height=5)


