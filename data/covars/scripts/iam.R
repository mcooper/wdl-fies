library(tidyverse)
library(zoo)
library(readxl)

iam <- read.csv('data/covars/rawdata/iam/SSP_IAM_V2_201811.csv') %>%
  filter(MODEL == "AIM/CGE", 
         SCENARIO == "SSP2-Baseline", 
         REGION != "World",
         grepl("(Agricultural Production)|(Land Cover)|Population", VARIABLE)) %>%
  mutate(REGION = gsub('.2','', REGION)) %>%
  select(-c(MODEL, SCENARIO, paste0("X", seq(2040, 2100, 10))))


region <- read_xlsx("data/covars/rawdata/iam/cmip6_iam_model_region_mapping.xlsx", 
                    sheet = "default_mapping") %>%
  select(ISO, R5_region) %>%
  rename(REGION = "R5_region")

#use the average of the region for all countries within the region
iamc <- merge(region, iam, all.y = T, by = "REGION") %>% 
  select(-REGION, -UNIT) %>%
  gather(YEAR, value, -VARIABLE, -ISO) %>%
  mutate(YEAR=as.numeric(substr(YEAR, 2, 5)),
         VARIABLE=case_when(grepl("Livestock", VARIABLE) ~ "livestock",
                            grepl("Crops", VARIABLE) ~ "crops_prod",
                            grepl("Forest", VARIABLE) ~ "forest",
                            grepl("Cropland", VARIABLE) ~ "cropland",
                            grepl("Built-up", VARIABLE) ~ "builtup",
                            grepl("Pasture", VARIABLE) ~ "pasture",
                            grepl("Population", VARIABLE) ~ "population")) %>%
  group_by(ISO, VARIABLE, YEAR) %>% 
  #Sum the two different "crops" variables
  summarize(value=sum(value)) %>%
  spread(VARIABLE, value) %>%
  #divide land covar vars by total to get fractions
  rowwise() %>%
  mutate(builtup = builtup/(builtup + cropland + forest + pasture),
         cropland = cropland/(builtup + cropland + forest + pasture),
         forest = forest/(builtup + cropland + forest + pasture),
         pasture = pasture/(builtup + cropland + forest + pasture),
         livestock = livestock/population,
         crops_prod = crops_prod/population) %>%
  #approax the years in between
  group_by(ISO) %>%
  complete(YEAR=2005:2030) %>%
  arrange(YEAR) %>%
  mutate(builtup=na.approx(builtup, na.rm=F),
         cropland=na.approx(cropland, na.rm=F),
         crops_prod=na.approx(crops_prod, na.rm=F),
         forest=na.approx(forest, na.rm=F),
         livestock=na.approx(livestock, na.rm=F),
         pasture=na.approx(pasture, na.rm=F)) %>%
  select(-population) %>%
  rename(ISO3=ISO)


write.csv(iamc, 'data/covars/results/iam.csv', row.names=F)


