library(ProjectTemplate)

setwd('~/wdl-fies')

load.project()

#Data on Matt's local computer from another project:
wealth <- read.csv('~/mortalityblob/dhs/hh_wealth_harmonized.csv')

#Get countries in both DHS and FIES dataset
wealth_sel <- wealth %>%
  mutate(ISO3 = countrycode(substr(code, 1, 2), 'dhs', 'iso3c')) %>%
  filter(ISO3 %in% fies_raw$ISO3)

#Get most recent surveys
w_recent <- wealth_sel %>%
  group_by(surveycode, ISO3) %>%
  summarize(survey_year = max(survey_year)) %>%
  group_by(ISO3) %>%
  filter(survey_year == max(survey_year))

#get gdl codes for each DHS survey
wealth_sp <- wealth_sel %>%
  ungroup %>%
  filter(surveycode %in% w_recent$surveycode) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326) %>%
  st_join(gdl, join=st_within) %>%
  st_drop_geometry 

#Get fraction in each region
wealth_sum <- wealth_sp %>%
  group_by(ISO3, GDLCODE) %>%
  summarize(wealth1=mean(wealth_quintile==1),
            wealth2=mean(wealth_quintile==2),
            wealth3=mean(wealth_quintile==3),
            wealth4=mean(wealth_quintile==4),
            wealth5=mean(wealth_quintile==5)) %>%
  na.omit

write.csv(wealth_sum, 'data/disag_vars/results/wealth_quintiles.csv', row.names=F)
