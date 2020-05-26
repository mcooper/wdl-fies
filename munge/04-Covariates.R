

ssp_hist <- read.csv('data/covars/SSP2_Vars_Historic.csv') %>%
  select(year=Year, iso3=iso3c, GDLCODE=GDLcode, ) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))