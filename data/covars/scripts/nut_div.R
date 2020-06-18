library(raster)
library(rgdal)
library(tidyverse)

sp <- readOGR('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')
nd <- raster('data/covars/rawdata/nutrition_diversity_mfad.asc')

e <- raster::extract(nd, sp, method='simple', fun=mean, na.rm=T,
             sp=TRUE, df=TRUE)

res <- e@data %>%
  select(GDLCODE=GDLcode, nutrition_diversity_mfad)

#For countries with partially missing data,
#Fill missing data with country mean
res <- res %>%
  rename(nut_div=nutrition_diversity_mfad) %>%
  mutate(ISO3=substr(GDLCODE, 1, 3)) %>%
  group_by(ISO3) %>%
  mutate(nut_div=ifelse(is.na(nut_div),
                        mean(nut_div, na.rm=T),
                        nut_div)) %>%
  filter(ISO3 %in% sp$iso_code)

#European Microstates to Luxembourg
#Tropical Island states to Solomon Islands
res$nut_div[res$ISO3 == 'AND'] <- mean(res$nut_div[res$ISO3 == 'LUX'], na.rm=T)
res$nut_div[res$ISO3 == 'ATG'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'BRB'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'COM'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'CPV'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'DMA'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'FSM'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'GRD'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'LIE'] <- mean(res$nut_div[res$ISO3 == 'LUX'], na.rm=T)
res$nut_div[res$ISO3 == 'MUS'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'STP'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'TON'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'VCT'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'VUT'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)
res$nut_div[res$ISO3 == 'WSM'] <- mean(res$nut_div[res$ISO3 == 'SLB'], na.rm=T)

#Use Qatar and Greece for Bahrain and Malta
res <- bind_rows(res,
                 data.frame(GDLCODE=c('BHRt', 'MLTt'),
                            ISO3=c('BHR', 'MLT'),
                            nut_div=c(mean(res$nut_div[res$ISO3=='QAT'], na.rm=T),
                                      mean(res$nut_div[res$ISO3=='GRC'], na.rm=T))))

write.csv(res, 'data/covars/results/nut_div.csv', row.names=F)
