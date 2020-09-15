library(tidyverse)

setwd('~/wdl-fies')

ours <- read.csv('data/preddat.csv') %>%
  group_by(YEAR) %>%
  summarize(Mod.Number = sum(fies.mod.pred*population, na.rm=T),
            Sev.Number = sum(fies.sev.pred*population, na.rm=T),
            Mod.Prevalence = weighted.mean(fies.mod.pred, w=population),
            Sev.Prevalence = weighted.mean(fies.sev.pred, w=population)) %>%
  gather(Var, Value, -YEAR) %>%
  separate(Var, c("Level", "Var"), '\\.') %>%
  mutate(source="Ours")

fao <- data.frame(YEAR = c(2014, 2015, 2016, 2017, 2018, 2019),
                  Mod.Prevalence = c(0.224, 0.225, 0.232, 0.248, 0.258, 0.259),
                  Sev.Prevalence = c(0.083, 0.079, 0.081, 0.086, 0.094, 0.097),
                  Mod.Number = c(1633.5, 1649.5, 1735.2, 1874.5, 1969.6, 2001.1)*1e6,
                  Sev.Number = c(602, 586, 605.5, 646.4, 717.5, 746)*1e6) %>%
  gather(Var, Value, -YEAR) %>%
  separate(Var, c("Level", "Var"), '\\.') %>%
  mutate(source="FAO")

fies <- bind_rows(ours, fao) %>%
  filter(YEAR < 2020, YEAR > 2013)

ggplot(fies) + 
  geom_line(aes(x=YEAR, y=Value, color=Level, linetype=source)) + 
  facet_wrap(. ~ Var, scales="free_y")

und <- data.frame(YEAR = c(2005, 2010, 2015, 2016, 2017, 2018, 2019, 2030),
                  Number = c(825.6, 668.2, 653.3, 657.6, 653.2, 678.1, 687.8, 841.4)*1e6,
                  Prevalence = c(0.126, 0.096, 0.089, 0.088, 0.087, 0.089, 0.089, 0.098)) %>%
  gather(Var, Value, -YEAR) %>%
  mutate(source = 'Undernourishment')

sdg2.1 <- bind_rows(ours %>% filter(Level == "Sev"), und)

ggplot(sdg2.1) + 
  geom_line(aes(x=YEAR, y=Value, color=source)) + 
  expand_limits(y=0) + 
  facet_wrap(. ~ Var, scales='free_y')




all <- bind_rows(ours, fao, und)
