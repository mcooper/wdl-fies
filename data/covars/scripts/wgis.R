setwd('~/wdl-fies')

library(readxl)
library(lubridate)

ref <- read.csv('data/covars/results/gdl_vars.csv') %>%
  group_by(ISO3, YEAR) %>%
  summarize(population=sum(population, na.rm=T))

sheets <- c("Political StabilityNoViolence",
            "VoiceandAccountability",
            "GovernmentEffectiveness",
            "RegulatoryQuality",
            "RuleofLaw",
            "ControlofCorruption")

read_wgi <- function(sheet){
  dat <- read_xlsx("wgidataset.xlsx", 
                         sheet = sheet, 
                         skip=14, 
                         na = "#N/A") %>%
    select(matches("Co|Estimate"))
  names(dat) <- c('country', 'code', paste0("X", c(1996, 1998, 2000, 2002:2018)))
  dat <- dat %>%
    mutate(ISO3=countrycode(country, 'country.name', 'iso3c')) %>%
    filter(ISO3 %in% ref$ISO3,
           country != c("Korea, Dem. Rep.")) %>%
    select(-country, -code) %>%
    gather(YEAR, value, -ISO3) %>%
    mutate(YEAR=as.numeric(substr(YEAR, 2, 5))) %>%
    arrange(desc(YEAR)) %>%
    group_by(ISO3) %>%
    fill(value) %>%
    arrange(YEAR) %>%
    group_by(ISO3) %>%
    fill(value)                  
  
 dat$var <- sheet

 dat
}

all <- data.frame()
for (sheet in sheets){
  res <- read_wgi(sheet)
  all <- bind_rows(all, res)
}

all <- all %>%
  mutate(var = case_when(var == "Political StabilityNoViolence" ~ "stability_noviolence", 
                         var == "VoiceandAccountability" ~ "voice_accountability",
                         var == "GovernmentEffectiveness" ~ "government_effectiveness", 
                         var == "RegulatoryQuality" ~ "regulatory_quality", 
                         var == "RuleofLaw" ~ "rule_of_law",
                         var == "ControlofCorruption" ~ "control_corruption")) %>%
  spread(var, value)

write.csv(all, 'data/covars/results/wgi_vars.csv', row.names=F)
