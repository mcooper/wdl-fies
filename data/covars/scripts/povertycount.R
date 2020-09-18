setwd('~/wdl-fies')

library(tidyverse)

#poverty headcount
pc <- read.csv('data/covars/rawdata/povertyclock/pc2020-08-18.csv') %>% 
  filter(daily_spending == "1.9", 
         age_group == "[00,05)",
         year <= 2030) %>%
  group_by(id, year) %>%
  summarize(hci=sum(hc)/sum(pop)) %>%
  select(ISO3=id, YEAR=year, hci) 

write.csv(pc, 'data/covars/results/povertycount.csv', row.names=F)
