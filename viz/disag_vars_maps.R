setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

#Get new weights
#Urban-Rural
ur <- read.csv('data/covars/results/urban-rural.csv') %>%
  select(GDLCODE, YEAR, rural_perc, urban_perc)

#Read in WorldPop Demographic data
dem <- read.csv('data/disag_vars/results/age_gender_all1418_rescale.csv') %>%
  #determine percentages for every group over 15 & get total
  select(GDLCODE, YEAR, matches(paste0(seq(15, 80, by=5), collapse='|'))) %>%
  mutate(total = rowSums(select(., matches(paste0(seq(15, 80, by=5), collapse='|'))))) %>%
  mutate_at(vars(matches(paste0(seq(15, 80, by=5), collapse='|'))), function(x){x/.$total}) %>%
  select(-total)

#Read in DHS Wealth Data
dhs <- read.csv('data/disag_vars/results/wealth_quintiles.csv') %>%
  select(-ISO3)

new_gdl <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)}, 
                  list(gdl,
                       ur %>% 
                         filter(YEAR == 2015) %>% 
                         select(GDLCODE, urban_perc),
                       dem %>% 
                         filter(YEAR == 2015) %>%
                         mutate(perc_female = rowSums(select(., contains('f_')))) %>%
                         select(GDLCODE, perc_female),
                       dem %>% 
                         filter(YEAR == 2015) %>%
                         mutate(perc_15.19 = rowSums(select(., contains('15')))) %>%
                         select(GDLCODE, perc_15.19),
                       dhs %>%
                         select(GDLCODE, perc_wealth1=wealth1))) %>%
  filter(iso_code %in% fies_raw$ISO3) %>%
  mutate_at(vars(matches('perc')), function(x){x*100})

cty <- ne_countries(returnclass='sf') %>%
  filter(region_wb != 'Antarctica')

#Urbanization
ggplot() + 
  geom_sf(data=cty, size=0.5) + 
  geom_sf(data=new_gdl, aes(fill=urban_perc), size=0.1) + 
  geom_sf(data=cty, size=0.5, fill='transparent') + 
  theme_void() + 
  scale_fill_gradientn(colours=c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Percent Urban')
ggsave('figures/disag/Percent Urban.png', width=8, height=3.5)

#Gender
ggplot() + 
  geom_sf(data=cty, size=0.5) + 
  geom_sf(data=new_gdl, aes(fill=perc_female), size=0.1) + 
  geom_sf(data=cty, size=0.5, fill='transparent') + 
  theme_void() + 
  scale_fill_gradientn(colours=c('#edf8fb','#b2e2e2','#66c2a4','#2ca25f','#006d2c')) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Percent Female')
ggsave('figures/disag/Percent Female.png', width=8, height=3.5)

#Age
ggplot() + 
  geom_sf(data=cty, size=0.5) + 
  geom_sf(data=new_gdl, aes(fill=perc_15.19), size=0.1) + 
  geom_sf(data=cty, size=0.5, fill='transparent') + 
  theme_void() + 
  scale_fill_gradientn(colours=c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d')) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Percent Aged 15-19')
ggsave('figures/disag/Percent Aged 15-19.png', width=8, height=3.5)

#Wealth
ggplot() + 
  geom_sf(data=cty, size=0.5) + 
  geom_sf(data=new_gdl, aes(fill=perc_wealth1), size=0.1) + 
  geom_sf(data=cty, size=0.5, fill='transparent') + 
  theme_void() + 
  scale_fill_gradientn(colours=c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f')) +
  coord_sf(crs='+proj=robin') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill='',
       title='Percent Poorest Quintile')
ggsave('figures/disag/Percent Poorest Quintile.png', width=8, height=3.5)

