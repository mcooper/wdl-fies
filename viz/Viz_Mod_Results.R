# setwd('~/wdl-fies'); library(ProjectTemplate); load.project(list(data_loading=T))

#Save outputs to wdl-fies-tex for manuscript
setwd('~/wdl-fies/docs/img')
#setwd('C:/Users/bmuel/Desktop/GitHub/wdl-fies/docs/img')

library(cowplot)
options(scipen=100)

############################
# visualize results
############################

########################
# Make Map
##########################

cty <- ne_countries(returnclass='sf') %>%
  filter(region_wb != 'Antarctica')

mapdat <- merge(gdl,
                preddat %>%
                  filter(YEAR %in% c(2010, 2020, 2030)) %>%
                  select(YEAR, GDLCODE, fies.mod.pred, fies.sev.pred) %>%
                  gather(outcome, value, -YEAR, -GDLCODE) %>%
                  mutate(FIES=ifelse(grepl('sev', outcome), 'Severe\n', 'Moderate\n')))

ggplot(mapdat) +
  geom_sf(aes(fill=value), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44"), 
                       breaks=seq(0, 1, by=0.25),
                       limits=c(0, max(mapdat$value)),
                       labels=function(x){paste0(x*100, '%')}) + 
  geom_sf(data=cty, color='#000000', fill=NA, size=0.15) + 
  coord_sf(crs='+proj=robin', expand=FALSE) + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        panel.spacing.x = unit(-0.5, "in")) + 
  labs(fill='') + 
  facet_grid(FIES ~ YEAR, switch='y')
ggsave('FullMap.pdf', width=10)
system('pdfcrop FullMap.pdf FullMap.pdf')


###########################
# Setup Reference Map
############################
region_cols <- c("High Income"="#279B48",
                 "Upper-Middle Income"="#02558B",
                 "Lower-Middle Income"="#F36D25",
                 "Low Income"="#EB1C2D")

regdat <- cty %>% 
  select(ISO3 = iso_a3, region_wb = income_grp) %>% #use world bank regions by income
  # rename and change order from high income to low income
  mutate(region_wb = case_when(region_wb == "1. High income: OECD" ~ "High Income",
                               region_wb == "2. High income: nonOECD" ~ "High Income",
                               region_wb == "3. Upper middle income" ~ "Upper-Middle Income",
                               region_wb == "4. Lower middle income" ~ "Lower-Middle Income",
                               region_wb == "5. Low income" ~ "Low Income"))


reg <- ggplot(regdat) + 
  geom_sf(aes(fill=region_wb), color=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  scale_fill_manual(values=region_cols) + 
  guides(fill=guide_legend(ncol=2)) + 
  labs(fill='')

#also change regions in preddat
preddat <- merge(preddat, st_drop_geometry(regdat)) %>%
  select(-region) %>%
  rename(region = "region_wb")

###############################################
# Rates over time, not just raw number
################################################

#get rate by YEAR
rates <- preddat %>%
  filter(YEAR >= 2010) %>%
  group_by(YEAR, region) %>%
  summarize(mod.rate=weighted.mean(fies.mod.pred, w=population, na.rm=T),
            sev.rate=weighted.mean(fies.sev.pred, w=population, na.rm=T),
            population=sum(population)) %>%
  gather(var, value, -YEAR, -region, -population) %>%
  mutate(var = ifelse(grepl('mod', var), "Moderate", "Severe")) %>%
  group_by(region, var) #%>%
  #mutate(value = rollapply(value, width=3, FUN=mean, align='center', partial=TRUE))

world <- rates %>%
  group_by(YEAR, var) %>%
  summarize(value=weighted.mean(value, w=population, na.rm=T)) %>%
  mutate(region='World')

ratesb <- bind_rows(rates, world)

region_cols2 <- c(region_cols, "World"="#4d4d4d")
size <- c(rep(1, 4), 2)
names(size) <- names(region_cols2)
lty <- c(rep(1, 4), 2)
names(lty) <- names(region_cols2)

(ratelines <- ggplot() +
  geom_line(data=ratesb, aes(x=YEAR, y=value, color=region, size=region, linetype=region), show.legend=F) +
  scale_color_manual(values=region_cols2) + 
  scale_size_manual(values=size) + 
  scale_linetype_manual(values=lty) + 
  theme_bw() + 
  theme(plot.margin=unit(c(-1, 0.5, 0, 0), 'lines'),
        panel.spacing=unit(2, 'lines')) + 
  scale_x_continuous(expand=c(0,0), labels=seq(2010, 2030, by=5)) + 
  scale_y_continuous(expand=expansion(mult=c(0,0.05), add=0), 
                     labels=function(x){paste0(x*100, '%')}) +
  labs(x='', y="",
       title="") +
  facet_grid(. ~ var))


(legendplot <- ggplot() + 
  geom_line(aes(x=c(0,1), y=c(0, 1), color=c('World', 'World'), 
                size=c('World', 'World'), linetype=c('World', 'World'))) +
  scale_size_manual(values=1) + 
  scale_color_manual(values='#4d4d4d') + 
  scale_linetype_manual(values=2) + 
  theme_bw()+ 
  labs(color='', linetype='', size='') + 
  theme(legend.key.width = unit(3, "cm"),
        legend.box.margin = ggplot2::margin(-5, -5, -5, -5),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA)))

pltleg <- get_legend(legendplot)
regleg <- get_legend(reg)

regnoleg <- ggplot(regdat) + 
  geom_sf(aes(fill=region_wb), color=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  scale_fill_manual(values=region_cols) + 
  guides(fill=FALSE) + 
  labs(fill='')

plot_grid(ratelines,
          plot_grid(regnoleg,
                    plot_grid(pltleg, 
                              regleg, 
                              ncol=1, 
                              rel_heights=c(1, 4)),
                    ncol=2,
                    rel_widths=c(1, 1.5)),
          ncol=1,
          rel_heights=c(2.5, 1))

ggsave('Rates.pdf', width=7, height=5)
#####################
# totals time series
###############

#get totals by YEAR
totals <- preddat %>%
  filter(YEAR >= 2010) %>%
  group_by(YEAR, region) %>%
  summarize(mod.total=sum(fies.mod.pred * (population), na.rm=t),
            sev.total=sum(fies.sev.pred * (population), na.rm=t)) %>%
  gather(var, value, -YEAR, -region) %>%
  mutate(var = ifelse(grepl('mod', var), "Moderate", "Severe")) %>%
  group_by(region, var)# %>%
  #mutate(value = rollapply(value, width=3, FUN=mean, align='center', partial=TRUE))

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
