
###-----------------------------------------------------------------------------------------------------------------
## SSP data

#########################
#past
###########################
ssp_past <- read.csv('data/covars/SSP2_Vars_Past.csv') 

ssp_past <- ssp_past %>%
  select(year=Year, iso3=iso3c, GDLCODE=GDLcode, grid_gdp, 
         edu_year = EdYears, rural = Rural, urban = Urban) %>%
  mutate(grid_gdp = log(grid_gdp),
         urban_perc = urban/(urban + rural)) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))

#Deal with missing data
#For areas where urban percentage is NaN, set to country mean
ssp_past$urban_perc[ssp_past$iso3 == 'VUT' & is.na(ssp_past$urban_perc)] <- mean(ssp_past$urban_perc[ssp_past$iso3 == 'VUT'], na.rm=T) 
ssp_past$urban_perc[ssp_past$iso3 == 'FSM' & is.na(ssp_past$urban_perc)] <- mean(ssp_past$urban_perc[ssp_past$iso3 == 'FSM'], na.rm=T) 

#Edu Year is missing at random, looks like it there for some years for almost all countries
ssp_past <- ssp_past %>% 
		#Make NANs NA
		mutate(edu_year=ifelse(is.nan(edu_year), NA, edu_year)) %>%
		#Fill in missing years with nearest chronological year
		arrange(GDLCODE, year) %>%
		group_by(GDLCODE) %>%
		mutate(edu_year=na.locf(na.locf(edu_year, na.rm=F), na.rm=F, fromLast=T))

#Grid_GDP currently missing for countries >50 degrees north
#So fill them in with a proxy country
rich_north <- c("BEL", "CAN", "DEU", "DNK", "EST", "FIN", "FRA", "GBR", "IRL", "ISL", "NLD", "NOR", "SWE", "USA")
poor_north <- c("BLR", "LTU", "LVA", "POL", "RUS")

#Set rich countries GDP equal to rural Germany
for (code in unique(ssp_past$GDLCODE[ssp_past$iso3 %in% rich_north & is.na(ssp_past$grid_gdp)])){
	ssp_past$grid_gdp[ssp_past$GDLCODE == code] <- ssp_past$grid_gdp[ssp_past$GDLCODE == "DEUr101"]
}

#Set poor countries GDP equal to rural Hungary
for (code in unique(ssp_past$GDLCODE[ssp_past$iso3 %in% poor_north & is.na(ssp_past$grid_gdp)])){
	ssp_past$grid_gdp[ssp_past$GDLCODE == code] <- ssp_past$grid_gdp[ssp_past$GDLCODE == "HUNr101"]
}

#For all other missing GDP data (in small islands), use national mean
ssp_past <- ssp_past %>%
	group_by(iso3) %>%
	mutate(grid_gdp=replace(grid_gdp, which(is.na(grid_gdp)), mean(grid_gdp, na.rm=TRUE)))

######################################
#future SSP data
#######################################
ssp_future <- read.csv('data/covars/SSP2_Vars_Future.csv') 

ssp_future <- ssp_future %>%
  select(year=Year, iso3=iso3c, GDLCODE=GDLcode, grid_gdp = GDP_PerCap, edu_year = mean_years_ed, 
         rural = rur_pop, urban = urb_pop) %>%
  mutate(grid_gdp = log(grid_gdp*100),
         urban_perc = urban/(urban + rural)) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))

#Missing data in Carribbean islands, Liechtenstein, and South Sudan
#For new, set South Sudan equal to CAR for gdp and ed
ssp_future <- ssp_future %>%
	group_by(year) %>%
	mutate(grid_gdp = replace(grid_gdp, which(iso3=='SSD'), mean(grid_gdp[iso3=='CAF'])),
				 edu_year = replace(edu_year, which(iso3=='SSD'), mean(edu_year[iso3=='CAF'])))


###-----------------------------------------------------------------------------------------------------------------
## extracted covariates

#all covariates extracted in the first round on the 22.04.2020
covar_ext <- read.csv('data/covars/GDL_extract_2020_04_22.csv')

#additional covar extracted on the 29.04.2020
livestock <- read.csv('data/covars/GDL_livestock_2020_04_29.csv')
malaria <- read.csv('data/covars/GDL_malaria_2020_04_29.csv')
#spei <- read.csv('data/covars/GDL_spei_2020_04_29.csv')


#manipulation
covar_ext$GDLCode <- substr(covar_ext$GDLCode, 42, 100)
covar_ext <- dcast(covar_ext, GDLCode + year ~ variable)
covar_ext <- covar_ext %>% dplyr::select(-c(bodycount, nutritiondiversity))

#livestock
colnames(livestock) <- c("GDLCode", "buffaloes", "cattle", "chickens", "ducks", "horses", "pigs", "sheep")
covar_ext <- merge(covar_ext, livestock, by = "GDLCode", all.x=T, all.y=F)

#malaria
malaria <- malaria %>% rename(mal_falciparum = "falciparum", mal_vivax = "vivax")
covar_ext <- merge(covar_ext, malaria, by = c("GDLCode", "year"), all.x=T, all.y=F)

# #Dont deal with SPEI right now
# #spei
# spei$year <- year(ymd(gsub('X', '', spei$date)))
# spei$month <- month(ymd(gsub('X', '', spei$date)))
# spei <- spei %>% select(-c(date))
# 
# covar_ext <- data.frame(covar_ext[rep(1:nrow(covar_ext), 12), ], month = rep(1:12, each = nrow(covar_ext)))
# covar_ext <- merge(covar_ext, spei, by = c("GDLCode", "year", "month"), all.x=T, all.y=F)

covar_ext <- covar_ext %>% 
	rename(GDLCODE = "GDLCode") %>%
	mutate(iso3 = substr(GDLCODE, 1, 3))

#Places with missing malaria data just have no malaria
covar_ext$mal_falciparum[is.na(covar_ext$mal_falciparum)] <- 0
covar_ext$mal_vivax[is.na(covar_ext$mal_vivax)] <- 0

#For missing data first pad NAs with most recent available year
covar_ext <- covar_ext %>% 
		#Make NANs NA
		#Fill in missing years with nearest chronological year
		arrange(GDLCODE, year) %>%
		group_by(GDLCODE) %>%
		mutate_each(funs(na.locf(na.locf(., na.rm=F), na.rm=F, fromLast=T))) %>%
		group_by(iso3, year) %>%
		mutate_each(funs(replace(., which(is.na(.)), mean(., na.rm=TRUE))))

#countrycode(covar_ext$iso3[apply(covar_ext, MARGIN=1, FUN=function(x){any(is.na(x))})] %>% unique, 'iso3c', 'country.name')
#All other missing data is islands or North Latitudes

###-----------------------------------------------------------------------------------------------------------------
## predict some of the extracted covariates with simple model

# v <- c("mal_falciparum", "mal_vivax", "market_dist", "assistance", "ag_pct_gdp", "imports_percap")
# data <- merge(covar_ext %>% select(year, GDLCODE, v), ssp_past, by = c("year", "GDLCODE"), all.x = T)
# 
# log.data <- data %>% 
#   mutate_at(vars(v), funs(log(.)))
# summary(log.data)
# 
# elast <- data.frame(var = v, int = NA, e = NA)
# elast$var <- as.character(elast$var)
# 
# #mal_falciparum
# m_mal_falciparum <- lm(mal_falciparum ~ grid_gdp, subset(log.data, mal_falciparum != is.na(mal_falciparum) & mal_falciparum > -Inf), na.action = na.omit); summary(m_mal_falciparum)
# elast$int[elast$var == "mal_falciparum"] <- m_mal_falciparum$coefficients[["(Intercept)"]]
# elast$e[elast$var == "mal_falciparum"] <- m_mal_falciparum$coefficients[["grid_gdp"]]
# 
# #mal_vivax
# m_mal_vivax <- lm(mal_vivax ~ grid_gdp, subset(log.data, mal_vivax != is.na(mal_vivax) & mal_vivax > -Inf), na.action = na.omit); summary(m_mal_vivax)
# elast$int[elast$var == "mal_vivax"] <- m_mal_vivax$coefficients[["(Intercept)"]]
# elast$e[elast$var == "mal_vivax"] <- m_mal_vivax$coefficients[["grid_gdp"]]
# 
# #market_dist
# m_market_dist <- lm(market_dist ~ grid_gdp, subset(log.data, market_dist != is.na(market_dist) & market_dist > -Inf), na.action = na.omit); summary(m_market_dist)
# elast$int[elast$var == "market_dist"] <- m_market_dist$coefficients[["(Intercept)"]]
# elast$e[elast$var == "market_dist"] <- m_market_dist$coefficients[["grid_gdp"]]
# 
# #assistance
# m_assistance <- lm(assistance ~ grid_gdp, subset(log.data, assistance != is.na(assistance) & assistance > -Inf), na.action = na.omit); summary(m_assistance)
# elast$int[elast$var == "assistance"] <- m_assistance$coefficients[["(Intercept)"]]
# elast$e[elast$var == "assistance"] <- m_assistance$coefficients[["grid_gdp"]]
# 
# #ag_pct_gdp
# m_ag_pct_gdp <- lm(ag_pct_gdp ~ grid_gdp, subset(log.data, ag_pct_gdp != is.na(ag_pct_gdp) & ag_pct_gdp > -Inf), na.action = na.omit); summary(m_ag_pct_gdp)
# elast$int[elast$var == "ag_pct_gdp"] <- m_ag_pct_gdp$coefficients[["(Intercept)"]]
# elast$e[elast$var == "ag_pct_gdp"] <- m_ag_pct_gdp$coefficients[["grid_gdp"]]
# 
# #imports_percap
# m_imports_percap <- lm(imports_percap ~ grid_gdp, subset(log.data, imports_percap != is.na(imports_percap) & imports_percap > -Inf), na.action = na.omit); summary(m_imports_percap)
# elast$int[elast$var == "imports_percap"] <- m_imports_percap$coefficients[["(Intercept)"]]
# elast$e[elast$var == "imports_percap"] <- m_imports_percap$coefficients[["grid_gdp"]]
# 
# covar_trend <- ssp_future %>% 
#   select(year, GDLCODE, grid_gdp)
# 
# covar_trend <- data.frame(covar_trend, matrix(nrow = nrow(covar_trend), ncol = 6))
# names(covar_trend)[4:9] <- v
# 
# covar_trend$mal_falciparum <- elast$int[elast$var == "mal_falciparum"] + elast$e[elast$var == "mal_falciparum"]*covar_trend$grid_gdp
# covar_trend$mal_vivax <- elast$int[elast$var == "mal_vivax"] + elast$e[elast$var == "mal_vivax"]*covar_trend$grid_gdp
# covar_trend$market_dist <- elast$int[elast$var == "market_dist"] + elast$e[elast$var == "market_dist"]*covar_trend$grid_gdp
# covar_trend$assistance <- elast$int[elast$var == "assistance"] + elast$e[elast$var == "assistance"]*covar_trend$grid_gdp
# covar_trend$ag_pct_gdp <- elast$int[elast$var == "ag_pct_gdp"] + elast$e[elast$var == "ag_pct_gdp"]*covar_trend$grid_gdp
# covar_trend$imports_percap <- elast$int[elast$var == "imports_percap"] + elast$e[elast$var == "imports_percap"]*covar_trend$grid_gdp
# 
# covar_trend[,3:9] <- exp(covar_trend[,3:9])


###-----------------------------------------------------------------------------------------------------------------
## cache files

cache('ssp_past')
cache('ssp_future')
cache('covar_ext')
#cache('covar_trend')



