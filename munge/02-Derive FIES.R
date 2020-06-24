#Load comparison data from FAO
compare <- FAOSTAT.data.4.14.2020 %>%
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

#Aggregate raw data to country level 
fies_cty <- fies_raw %>% 
  group_by(ISO3, YEAR) %>% 
  summarize(fies.mod = weighted.mean(Prob_Mod_Sev, w = wt, na.rm=T),
            fies.sev = weighted.mean(Prob_sev, w = wt, na.rm=T)) %>%
  mutate(fies.mod.3yr = rollapply(fies.mod, width=3, FUN=mean, na.pad=T),
         fies.sev.3yr = rollapply(fies.sev, width=3, FUN=mean, na.pad=T)) %>%
  merge(compare, all.x=T, all.y=F)

#Disaggregate by urban-rural
fies_ur <- fies_raw %>% 
  merge(regions, all.x=T, all.y=F) %>%
  filter(!Area %in% c('(Refused)', 'Dont_know'),
         !is.na(Area)) %>%
  group_by(region, Area) %>% 
  summarize(fies.reg.mod = weighted.mean(Prob_Mod_Sev, w = wt, na.rm=T),
            fies.reg.sev = weighted.mean(Prob_sev, w = wt, na.rm=T)) %>%
  mutate(Area = case_when(grepl('Urban', Area) ~ '.urb',
                          TRUE ~ '.rur')) %>%
  gather(key, value, -region, -Area) %>%
  mutate(key=paste0(key, Area),
         Area=NULL) %>%
  spread(key, value) %>%
  mutate(ur.ratio.sev = fies.reg.sev.urb/fies.reg.sev.rur,
         ur.ratio.mod = fies.reg.mod.urb/fies.reg.mod.rur)

cache('fies_cty')
cache('fies_ur')
cache('regions')

