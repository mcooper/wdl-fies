
###-----------------------------------------------------------------------------------------------------------------
## SSP data

#past
ssp_past <- read.csv('data/covars/SSP2_Vars_Past.csv') 

ssp_past <- ssp_past %>%
  select(year=Year, iso3=iso3c, GDLCODE=GDLcode, elevation, mean_annual_precip, roughness, grid_gdp, 
         edu_year = EdYears, rural = Rural, urban = Urban) %>%
  mutate(grid_gdp = log(grid_gdp),
         urban_perc = urban/(urban + rural)) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))


#future SSP data
ssp_future <- read.csv('data/covars/SSP2_Vars_Future.csv') 

ssp_future <- ssp_future %>%
  select(year=Year, iso3=iso3c, GDLCODE=GDLcode, gdp_capita = GDP_PerCap, edu_year = mean_years_ed, 
         rural = rur_pop, urban = urb_pop) %>%
  mutate(grid_gdp = log(gdp_capita*100),
         urban_perc = urban/(urban + rural)) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))


###-----------------------------------------------------------------------------------------------------------------
## extracted covariates

#all covariates extracted in the first round on the 22.04.2020
covar_ext <- read.csv('data/covars/GDL_extract_2020_04_22.csv')

#additional covar extracted on the 29.04.2020
livestock <- read.csv('data/covars/GDL_livestock_2020_04_29.csv')
malaria <- read.csv('data/covars/GDL_malaria_2020_04_29.csv')
spei <- read.csv('data/covars/GDL_spei_2020_04_29.csv')


#manipulation
covar_ext$GDLCode <- substr(covar_ext$GDLCode, 42, 100)
covar_ext <- dcast(covar_ext, GDLCode + year ~ variable)
covar_ext <- covar_ext %>% dplyr::select(-c(bodycount, nutritiondiversity))

#livestock
colnames(livestock) <- c("GDLCode", "buffaloes", "cattle", "chickens", "ducks", "horses", "pigs", "sheep")
covar_ext <- merge(covar_ext, livestock, by = "GDLCode")

#malaria
malaria <- malaria %>% rename(mal_falciparum = "falciparum", mal_vivax = "vivax")
covar_ext <- merge(covar_ext, malaria, by = c("GDLCode", "year"))

#spei
spei$year <- year(ymd(gsub('X', '', spei$date)))
spei$month <- month(ymd(gsub('X', '', spei$date)))
spei <- spei %>% select(-c(date))

covar_ext <- data.frame(covar_ext[rep(1:nrow(covar_ext), 12), ], month = rep(1:12, each = nrow(covar_ext)))
covar_ext <- merge(covar_ext, spei, by = c("GDLCode", "year", "month"))

covar_ext <- covar_ext %>% rename(GDLCODE = "GDLCode")

###-----------------------------------------------------------------------------------------------------------------
## cache files

cache('ssp_past')
cache('ssp_future')
cache('covar_ext')



