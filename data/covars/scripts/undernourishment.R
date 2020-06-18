library(tidyverse)
library(countrycode)

dat <- read.csv('data/covars/rawdata/undernourishment/API_SN.ITK.DEFC.ZS_DS2_en_csv_v2_1120870.csv', skip=4) %>%
  mutate(ISO3 = countrycode(Country.Name, 'country.name', 'iso3c')) %>%
  dplyr::select(-Country.Code, -Indicator.Name, -Indicator.Code, -Country.Name) %>%
  gather(YEAR, undernourishment, -ISO3) %>%
  mutate(YEAR = as.numeric(gsub('X', '', YEAR))) %>%
  filter(!is.na(YEAR),
         !is.na(ISO3),
         YEAR >= 1990) %>%
  arrange(desc(YEAR)) %>%
  group_by(ISO3) %>%
  fill(undernourishment) %>%
  arrange(YEAR) %>%
  group_by(ISO3) %>%
  fill(undernourishment)

# TOO MANY MISSING

# [1] "Andorra"                          "Antigua & Barbuda"
#  [3] "Burundi"                          "Bahrain"
#  [5] "Bahamas"                          "Bhutan"
#   [7] "Congo - Kinshasa"                 "Comoros"
#   [9] "Eritrea"                          "Micronesia (Federated States of)"
#   [11] "Equatorial Guinea"                "Grenada"
#   [13] "Libya"                            "Liechtenstein"
#   [15] "Moldova"                          "Papua New Guinea"
#   [17] "Palestinian Territories"          "Qatar"
#   [19] "Singapore"                        "Somalia"
#   [21] "South Sudan"                      "Syria"
#   [23] "Tajikistan"                       "Tonga"

# SKIP THIS VAR FOR NOW

#write.csv(dat, 'data/covars/results/undernourishment.csv', row.names=F)
