setwd('~/wdl-fies');library(ProjectTemplate);load.project()


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

preddat2 <- preddat %>%
  group_by(ISO3, YEAR) %>%
  summarize(fies.mod = weighted.mean(fies.mod.pred, w=population),
            fies.sev = weighted.mean(fies.sev.pred, w=population)) %>%
  group_by(ISO3) %>%
  arrange(YEAR) %>%
  mutate(fies.mod.3yr.pred = zoo::rollmean(fies.mod, k=3, fill=NA, na.rm=T),
         fies.sev.3yr.pred = zoo::rollmean(fies.sev, k=3, fill=NA, na.rm=T))

m <- merge(compare, preddat2, all.x=T, all.y=F) %>%
  na.omit

plot(m$fies.mod.3yr.fao, m$fies.mod.3yr.pred)
cor(m$fies.mod.3yr.fao, m$fies.mod.3yr.pred)

plot(m$fies.sev.3yr.fao, m$fies.sev.3yr.pred)
cor(m$fies.sev.3yr.fao, m$fies.sev.3yr.pred)

#Examine large errors
m$fies.mod.error <- m$fies.mod.3yr.fao - m$fies.mod.3yr.pred
m$fies.sev.error <- m$fies.sev.3yr.fao - m$fies.sev.3yr.pred

bad <- m %>% 
  filter(abs(fies.mod.error) > 0.05 | abs(fies.sev.error) > 0.05) %>%
  mutate(country = countrycode(ISO3, 'iso3c', 'country.name')) %>%
  filter(YEAR == 2016) %>%
  select(country, `FAO Moderate`=fies.mod.3yr.fao, 
         `WDL Moderate`=fies.mod.3yr.pred, `Error Moderate`=fies.mod.error,
         `FAO Severe`=fies.sev.3yr.fao, `WDL Severe`=fies.sev.3yr.pred,
         `Error Severe`=fies.sev.error) %>%
  arrange(`Error Moderate`) %>%
  mutate_at(vars(matches('evere|oder')), function(x) {paste0(round(x*100), '%')})



