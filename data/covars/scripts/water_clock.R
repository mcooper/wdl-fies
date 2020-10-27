setwd('~/wdl-fies')

library(tidyverse)
library(zoo)

#water scarcity
fs <- list.files('data/covars/rawdata/wc', pattern='csv$', recursive = T, full.names = T)
wc_list <- list()
i <- 1

for (f in fs){
  n <- gsub('data/covars/rawdata/wc/','', f)
  n <- gsub('.csv','', n)
  f <- read.csv(f) %>% select(GDLcode, Mean)
  colnames(f) <- c("GDLCODE", n)
  wc_list[[i]] <- f
  i <- i + 1
}

wc <- Reduce(merge, wc_list) %>%
  select(!starts_with("popwci500")) %>%
  select(!starts_with("popwci1700"))


# calculate the share of people living in areas with less than 1000m3 per capita per year
wc$popwci1000_2010spst <- wc$popwci1000_2010spst/wc$totalpop_2010spst #2010
wc$popwci1000_2020spst <- wc$popwci1000_2020spst/wc$totalpop_2020spst #2020
wc$popwci1000_2030spst <- wc$popwci1000_2030spst/wc$totalpop_2030spst #2030


wc <- wc %>% 
  select(!starts_with("totalpop")) %>%
  pivot_longer(cols = starts_with("popwci1000"), 
               names_to = "year", 
               values_to = "ws_share") %>%
  mutate(year = as.integer(substr(year, 12, 15)))


wc <- wc %>%
  select(GDLCODE, YEAR=year, ws_share) %>%
  group_by(GDLCODE) %>%
  arrange(YEAR) %>%
  complete(YEAR=2010:2030) %>%
  ungroup() %>%
  mutate(ws_share=na.approx(ws_share)) %>%
  na.omit

write.csv(wc, 'data/covars/results/water_clock.csv', row.names=F)
