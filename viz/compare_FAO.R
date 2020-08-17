
#Load comparison data from FAO
compare <- read.csv('data/FAOSTAT_data_4-14-2020.csv') %>%
  select(Area, Year, Value, Item) %>%
  transmute(country = Area,
            YEAR = as.numeric(substr(Year, 1, 4)) + 1,
            value = as.numeric(Value)/100,
            item = case_when(grepl('moderate', Item) ~ 'fies.mod.3yr.fao',
                             TRUE ~ 'fies.sev.3yr.fao')) %>%
  spread(item, value) %>%
  mutate(ISO3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(-country) %>%
  na.omit

#Get national level rates
nat <- fies_subnat %>%
  merge(covars %>% 
          select(ISO3, GDLCODE, YEAR, population)) %>%
  group_by(ISO3, YEAR) %>%
  summarize(fies.mod = weighted.mean(fies.mod, wt=population),
            fies.sev = weighted.mean(fies.sev, wt=population)) %>%
  arrange(YEAR) %>%
  mutate(fies.mod.3yr = rollapply(fies.mod, width=3, FUN=mean, na.pad=T),
         fies.sev.3yr = rollapply(fies.sev, width=3, FUN=mean, na.pad=T))

comb <- merge(compare, nat)

plot(comb$fies.mod.3yr, comb$fies.mod.3yr.fao)
plot(comb$fies.sev.3yr, comb$fies.sev.3yr.fao)

ggplot(comb) + 
  geom_point(aes(x=fies.sev.3yr.fao,
                 y=fies.sev.3yr)) + 
  labs(x='Published Estimates of Severe Food Insecurity',
       y='Manually Calculated Estimates of Severe Food Insecurity')
ggsave('figures/FAO Compare - Severe.png')

ggplot(comb) + 
  geom_point(aes(x=fies.mod.3yr.fao,
                 y=fies.mod.3yr)) + 
  labs(x='Published Estimates of Moderate Food Insecurity',
       y='Manually Calculated Estimates of Moderate Food Insecurity')
ggsave('figures/FAO Compare - Moderate.png')
