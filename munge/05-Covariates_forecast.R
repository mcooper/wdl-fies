

###############################
## covariates forecast model ##
###############################

###-----------------------------------------------------------------------------------------------------------------
## Gini, HDI

#install.packages("openxlsx")
library(openxlsx)

#gini
gini <- read.xlsx("data/covars_forecast/SSP/Gini_projections_SSPs.xlsx", sheet = "projected_ginis_full-set")

gini <- gini %>% 
  filter(scenario == "SSP2", year %in% 2011:2032) %>% 
  select(-scenario)

gini <- reshape2::melt(gini, id.vars = "year")

gini <- gini %>% 
  rename(iso3 = "variable", gini = "value") %>%
  mutate(gini = gini/100, iso3 = as.character(iso3)) %>%
  select(iso3, year, gini)

gini <- gini %>% filter(year %in% c(2010:2020, 2025, 2030))


#hdi
hdi <- read.xlsx("data/covars_forecast/HDI/HDI-SSPs.xlsx", sheet = "Tabelle1")

hdi$iso3 <- countrycode(gsub(' - \\d+','', hdi$obs), origin = "country.name", destination = "iso3c")
hdi$year <- as.numeric(paste0("20", gsub('\\D','', hdi$obs)))

hdi <- hdi %>% 
  rename(hdi = "HDI_SSP2") %>%
  select(year, iso3, hdi) %>%
  filter(year %in% c(2010, 2015, 2020, 2025, 2030, 2035))

#approax the years in between
hdi <- data.table(hdi)
hdi <- rbind(hdi, hdi[, .(year = setdiff(2010:2035, c(2010, 2015, 2020, 2025, 2030, 2035)), hdi = NA), by = .(iso3)])
hdi <- hdi[order(year), .SD, by = iso3]

hdi[, hdi := na.approx(hdi) , by = iso3]

hdi <- hdi %>% filter(year %in% c(2010:2020, 2025, 2030))

# #also include data from here for past years
# #http://hdr.undp.org/en/content/table-2-human-development-index-trends-1990%E2%80%932018
# hdi_un <- read.csv('data/covars_forecast/HDI/HDI_UNDP.csv', skip = 1, sep = ",") %>%
#   mutate(iso3 = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#   select(-c(HDI.Rank..2018., Country)) %>%
#   filter(!is.na(iso3))
# 
# hdi_un[hdi_un == ".."] <- NA
# colnames(hdi_un) <- c(1990:2018, "iso3")
# 
# hdi_un <- reshape2::melt(hdi_un, id.vars = "iso3", variable.name = "year", value.name = "hdi")
# 
# hdi <- rbind(hdi, hdi_un)
# hdi <- hdi[order(year), .(year, hdi), by = iso3]

###-----------------------------------------------------------------------------------------------------------------
## IAM (Integrated assessment modelling), use Agriculture and Land Cover variables

iam <- read.csv('data/covars_forecast/SSP/SSP_IAM_V2_201811.csv') %>%
  filter(MODEL == "AIM/CGE", SCENARIO == "SSP2-Baseline", REGION != "World",
         VARIABLE %in% c("Agricultural Production|Livestock", "Agricultural Production|Crops|Non-Energy", "Agricultural Production|Crops|Energy",
                         "Land Cover|Forest", "Land Cover|Cropland", "Land Cover|Built-up Area")) %>%
  mutate(REGION = gsub('.2','', REGION)) %>%
  select(-c(MODEL, SCENARIO, paste0("X", seq(2050, 2100, 10))))


region <- read.xlsx("data/covars_forecast/SSP/cmip6_iam_model_region_mapping.xlsx", sheet = "default_mapping") %>%
  select(ISO, R5_region) %>%
  rename(REGION = "R5_region")

#use the average of the region for all countries within the region
iam <- merge(region, iam, all.y = T, by = "REGION") %>% select(-REGION)

#check units
iam <- iam %>% select(-UNIT)

colnames(iam) <- c("iso3", "var", c(2005, 2010, 2020, 2030, 2040))

iam <- reshape2::melt(iam, id.vars = c("iso3", "var"), variable.name = "year")
iam <- reshape2::dcast(iam, iso3 + year ~ var)

iam <- iam %>% 
  rename(livestock = "Agricultural Production|Livestock", 
                      crops_nonenergy = "Agricultural Production|Crops|Non-Energy", 
                      crops_energy = "Agricultural Production|Crops|Energy",
                      forest = "Land Cover|Forest", 
                      cropland = "Land Cover|Cropland", 
                      builtup = "Land Cover|Built-up Area") %>%
  mutate(crops_prod = crops_nonenergy + crops_energy) %>%
  select(-c(crops_nonenergy, crops_energy))
  

#approax the years in between
iam <- data.table(iam)
iam <- rbind(iam, iam[, .(year = setdiff(2005:2040, c(2005, 2010, 2020, 2030, 2040)), 
                          livestock = NA, crops_prod = NA, forest = NA, cropland = NA, builtup = NA), by = .(iso3)])
iam$year <- as.numeric(as.character(iam$year))

iam <- iam[order(year), .SD, by = iso3]

iam[, livestock := na.approx(livestock) , by = iso3]
iam[, crops_prod := na.approx(crops_prod) , by = iso3]
iam[, forest := na.approx(forest) , by = iso3]
iam[, cropland := na.approx(cropland) , by = iso3]
iam[, builtup := na.approx(builtup) , by = iso3]

iam <- iam %>% filter(year %in% c(2010:2020, 2025, 2030))


###-----------------------------------------------------------------------------------------------------------------
## GDP and education

# edu <- read.csv('data/covars_forecast/SSP/SspDb_country_data_2013-06-12.csv')
# 
# pop <- edu %>% 
#   filter(MODEL == "IIASA-WiC POP", SCENARIO == "SSP2_v9_130115") %>%
#   filter(!str_detect(VARIABLE, "Education")) %>%
#   select(REGION,VARIABLE, paste0("X", seq(2010, 2040, 5)))
# 
# colnames(pop) <- c("iso3", "var", c(2010, 2015, 2020, 2025, 2030, 2035, 2040))
# 
# pop$var[grepl("Aged95-99", pop$var, fixed = TRUE) == T] <- "95-99"
# pop$var[grepl("Aged100+", pop$var, fixed = TRUE) == T] <- "100+"
# 
# pop$var[pop$var == "Population"] <- NA
# pop$var[grepl("Aged0-4", pop$var, fixed = TRUE) == T] <- NA
# pop$var[grepl("Aged5-9", pop$var, fixed = TRUE) == T] <- NA
# pop$var[grepl("Aged10-14", pop$var, fixed = TRUE) == T] <- NA
# pop$var[pop$var == "Population|Male"] <- NA
# pop$var[pop$var == "Population|Female"] <- NA
# 
# pop <- pop %>% filter(!is.na(var))
# 
# pop$var <- gsub("Population|Male|Aged", "", fixed = T, pop$var)
# pop$var <- gsub("Population|Female|Aged", "", fixed = T, pop$var)
# 
# pop <- reshape2::melt(pop, id.vars = c("iso3", "var"), variable.name = "year")
# pop <- pop %>% rename(age = "var", pop = "value") %>% mutate(pop = pop*1000000)
# pop <- reshape2::dcast(pop, iso3 + year ~ age, value.var = "pop", fun.aggregate = sum)
# 
# pop$pop_15plus <- apply(pop[,3:20], 1, FUN = sum)
# 
# # pop_share <- pop
# # pop <- reshape2::melt(pop, id.vars = c("iso3", "year"), variable.name = "age", value.name = "pop")
# # 
# # pop_share[,3:20] <- apply(pop_share[,3:20], 2, function(x) {x/pop_share$pop_15plus})
# # pop_share <- pop_share %>% select(-pop_15plus)
# # pop_share <- reshape2::melt(pop_share, id.vars = c("iso3", "year"), variable.name = "age", value.name = "pop_share")
# 
# pop <- pop %>% select(iso3, year, pop_15plus)
# 
# 
# 
# edu <- edu %>% 
#   filter(MODEL == "IIASA-WiC POP", SCENARIO == "SSP2_v9_130115")%>%
#   filter(str_detect(VARIABLE, "Education")) %>%
#   select(REGION,VARIABLE, paste0("X", seq(2010, 2040, 5)))
# 
# 
# colnames(edu) <- c("iso3", "var", c(2010, 2015, 2020, 2025, 2030, 2035, 2040))
# 
# edu$var[grepl("Aged0-4", edu$var, fixed = TRUE) == T] <- NA
# edu$var[grepl("Aged5-9", edu$var, fixed = TRUE) == T] <- NA
# edu$var[grepl("Aged10-14", edu$var, fixed = TRUE) == T] <- NA
# edu$var[grepl("No Education", edu$var, fixed = TRUE) == T] <- NA
# 
# edu <- edu %>% filter(!is.na(var))
# 
# edu$var[grepl("Primary Education", edu$var, fixed = TRUE) == T] <- "pri_edu"
# edu$var[grepl("Secondary Education", edu$var, fixed = TRUE) == T] <- "sec_edu"
# edu$var[grepl("Tertiary Education", edu$var, fixed = TRUE) == T] <- "ter_edu"
# 
# edu <- reshape2::melt(edu, id.vars = c("iso3", "var"), variable.name = "year")
# edu$value <- round(edu$value*1000000, digits = 0)
# 
# edu <- reshape2::dcast(edu, iso3 + year ~ var, sum)
# edu <- merge(edu, pop, by = c("iso3", "year"))
# 
# edu <- data.table(edu)
# 
# edu <- rbind(edu, edu[, .(year = setdiff(2010:2040, c(2010, 2015, 2020, 2025, 2030, 2035, 2040)), 
#                           pri_edu = NA, sec_edu = NA, ter_edu = NA, pop_15plus = NA), by = .(iso3)])
# 
# edu$year <- as.numeric(as.character(edu$year))
# 
# edu <- edu[order(year), .SD, by = iso3]
# 
# edu[, pri_edu := round(na.approx(pri_edu), digits = 0) , by = iso3]
# edu[, sec_edu := round(na.approx(sec_edu), digits = 0) , by = iso3]
# edu[, ter_edu := round(na.approx(ter_edu), digits = 0) , by = iso3]
# edu[, pop_15plus := round(na.approx(pop_15plus), digits = 0) , by = iso3]
# 
# edu <- data.frame(edu)
# 
# edu[,3:5] <- apply(edu[,3:5], 2, FUN = function(x) {x/edu$pop_15plus})
# 
# edu_share <- edu %>% 
#   select(iso3, year, pri_edu, sec_edu, ter_edu) %>% 
#   filter(year %in% c(2010:2020, 2025, 2030))
# 
# edu <- edu %>% select(-pop_15plus)
# edu <- reshape2::melt(edu, id.vars = c("iso3", "year"), variable.name = "class", value.name = "share")
# 
# #http://uis.unesco.org/sites/default/files/documents/international-standard-classification-of-education-isced-2011-en.pdf
# edu$duration <- NA
# edu$duration[edu$class == "pri_edu"] <- 9
# edu$duration[edu$class == "sec_edu"] <- 12
# edu$duration[edu$class == "ter_edu"] <- 16
# 
# edu$edu_years <- edu$share*edu$duration
# edu <- data.table(edu)
# 
# edu <- edu[, .(edu_years = sum(edu_years)), by = .(iso3, year)]
# edu <- edu %>% filter(year %in% c(2010:2020, 2025, 2030))

#use mean years of schooling
#http://dataexplorer.wittgensteincentre.org/wcde-v2/
edu <- read.csv('data/covars_forecast/SSP/wcde_edu.csv', skip = 8)

pop <- read.csv('data/covars_forecast/SSP/wcde_pop.csv', skip = 8)


edu <- merge(edu, pop, by = c("Area", "Year", "Age"), all.x = T) %>%
  mutate(Population = Population*1000) %>% 
  mutate(iso3 = countrycode(Area, origin = "country.name", destination = "iso3c")) %>%
  select(iso3 , year=Year, edu=Years, pop=Population) %>%
  as.data.table

edu <- edu[year >= 2010 & year <= 2040, .(edu_years = sum((edu*pop))/sum(pop)), by = .(iso3, year)]


edu <- rbind(edu, edu[, .(year = setdiff(2010:2040, c(2010, 2015, 2020, 2025, 2030, 2035, 2040)), edu_years = NA), by = .(iso3)])

edu$year <- as.numeric(as.character(edu$year))

edu <- edu[order(year), .SD, by = iso3]
edu$edu_years <- na.approx(edu$edu_years)

edu <- edu %>%
  select(iso3, year, edu_years) %>%
  filter(year %in% c(2010:2020, 2025, 2030))


#GDP
gdp <- readRDS("data/covars_forecast/PC/X8_PC_nat_2020-04-28.RDS")$macro.data %>%
  select(ccode, year, GDP.PC.PPP) %>%
  rename(iso3 = "ccode", gdp_pc = "GDP.PC.PPP")

#gdp <- gdp %>% filter(year %in% c(2010:2020, 2025, 2030))  

###-----------------------------------------------------------------------------------------------------------------
## poverty clock and water scarcity clock

#poverty headcount
pc <- read.csv('data/covars_forecast/PC/pc2020-04-28.csv') %>% filter(daily_spending == "1.9") %>% as.data.table

pc[, hci := sum(hc)/sum(pop), by = c("id", "year")]

pc <- pc %>% 
  filter(age_group == "[00,05)", gender == "male") %>% 
  select(id, year, hci) %>% 
  rename(iso3 = "id")

pc <- pc %>% filter(year %in% c(2010:2020, 2025, 2030))

#water scarcity
fs <- list.files('data/covars_forecast/WC', pattern='csv$', recursive = T, full.names = T)
wc_list <- list()
i <- 1

for (f in fs){
  n <- gsub('data/covars_forecast/WC/','', f)
  n <- gsub('.csv','', n)
  f <- read.csv(f) %>% select(GDLcode, Mean)
  colnames(f) <- c("GDLCODE", n)
  wc_list[[i]] <- f
  i <- i + 1
}

wc <- Reduce(merge, wc_list)

#2010
wc$popwci500_2010spst <- wc$popwci500_2010spst/wc$totalpop_2010spst
wc$popwci1000_2010spst <- wc$popwci1000_2010spst/wc$totalpop_2010spst
wc$popwci1700_2010spst <- wc$popwci1700_2010spst/wc$totalpop_2010spst

#2020
wc$popwci500_2020spst <- wc$popwci500_2020spst/wc$totalpop_2020spst
wc$popwci1000_2020spst <- wc$popwci1000_2020spst/wc$totalpop_2020spst
wc$popwci1700_2020spst <- wc$popwci1700_2020spst/wc$totalpop_2020spst

#2030
wc$popwci500_2030spst <- wc$popwci500_2030spst/wc$totalpop_2030spst
wc$popwci1000_2030spst <- wc$popwci1000_2030spst/wc$totalpop_2030spst
wc$popwci1700_2030spst <- wc$popwci1700_2030spst/wc$totalpop_2030spst

wc <- wc %>% select(!starts_with("totalpop"))

wc <- reshape2::melt(wc, id.vars = "GDLCODE", variable.name = "var")
wc$var <- as.character(wc$var)
wc$year[grepl("2010", wc$var, fixed = TRUE) == T] <- 2010
wc$year[grepl("2020", wc$var, fixed = TRUE) == T] <- 2020
wc$year[grepl("2030", wc$var, fixed = TRUE) == T] <- 2030

wc$var <- gsub("pop", "", wc$var)
wc$var <- gsub("_\\d\\d\\d\\dspst", "", wc$var)

wc <- reshape2::dcast(wc, GDLCODE + year ~ var)

wc <- data.table(wc)
wc <- rbind(wc, wc[, .(year = setdiff(2010:2030, c(2010, 2020, 2030)), 
                       wci1000 = NA, wci1700 = NA, wci500 = NA), by = .(GDLCODE)])

wc <- wc[order(year), .SD, by = GDLCODE]

wc[, wci500 := na.approx(wci500)]
wc[, wci1000 := na.approx(wci1000)]
wc[, wci1700 := na.approx(wci1700)]

#norm to 1 and create index
# wc$wci_index <- (1/(wc$wci500+wc$wci1000+wc$wci1700))*(3*wc$wci500 + 2*wc$wci1000 + 1*wc$wci1700)
wc$wci_index <- (3*wc$wci500 + 2*wc$wci1000 + 1*wc$wci1700)
wc$wci_index[(wc$wci500+wc$wci1000+wc$wci1700) == 0] <- 0

# mapdat <- merge(gdl, wc %>% select(GDLCODE, year, wci_index), all.x=T, all.y=F)	%>%
#   filter(year == 2020)
# 
# countries <- ne_countries(returnclass='sf')
# ggplot(subset(mapdat, GDLCODE == "RUSr107")) +
#   geom_sf(aes(fill=wci_index), color=NA) +
#   geom_sf(data=countries, color='#000000', fill=NA) +
#   coord_sf(crs='+proj=robin') +
#   theme_void()

wc <- wc %>% select(GDLCODE, year, wci500, wci1000, wci1700, wci_index) %>% filter(year %in% c(2010:2020, 2025, 2030))


###-----------------------------------------------------------------------------------------------------------------
## historic SSP data

ssp_past <- read.csv('data/covars_forecast/SSP/SSP2_Vars_Past.csv')

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


###-----------------------------------------------------------------------------------------------------------------
## future SSP data

ssp_future <- read.csv('data/covars_forecast/SSP/SSP2_Vars_Future.csv')

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

#only use urban for now
rur_urb <- rbind(ssp_past %>% select(iso3, GDLCODE, year, rural, urban, urban_perc), ssp_future %>% select(iso3, GDLCODE, year, rural, urban, urban_perc))

rur_urb$population <- round(rur_urb$rural+rur_urb$urban, digits = 0)

rur_urb <- rur_urb %>% filter(year %in% c(2010:2020, 2025, 2030))

###-----------------------------------------------------------------------------------------------------------------
## predict some of the extracted covariates with simple model

ag_pct_gdp <- read.csv('data/covars_nowcast/results/ag_pct_gdp.csv') %>%
  select(iso3=ISO3, year=YEAR, ag_pct_gdp)

imports_percap <- read.csv('data/covars_nowcast/results/imports_percap.csv') %>%
  select(iso3=ISO3, year=YEAR, imports_percap) 

mal_falciparum <- read.csv('data/covars_nowcast/results/malaria.csv') %>%
  select(GDLCODE, year=YEAR, mal_falciparum) 

pop <- read.csv('data/covars_nowcast/results/urban-rural.csv') %>%
  mutate(pop = rural+urban) %>%
  select(GDLCODE, year=YEAR, pop)

mal_falciparum <- merge(mal_falciparum, pop) %>%
  mutate(iso3 = substr(GDLCODE, 1, 3)) %>% as.data.frame %>%
  select(-GDLCODE) %>%
  group_by(iso3, year) %>%
  summarize_at(vars("mal_falciparum"), funs(weighted.mean(., w = pop)))
  
mal_falciparum <- data.frame(mal_falciparum) %>%
  select(iso3, year, mal_falciparum) 

# v <- c("mal_falciparum", "mal_vivax", "market_dist", "assistance", "ag_pct_gdp", "imports_percap")

data <- Reduce(function(x, y){left_join(x, y)}, list(mal_falciparum, ag_pct_gdp, imports_percap, gdp))

data <- data %>% filter(!is.na(gdp_pc))

summary(data)

log.data <- data %>%
  mutate_at(vars("gdp_pc", "mal_falciparum", "ag_pct_gdp", "imports_percap"), funs(log(.)))
summary(log.data)

elast <- data.frame(var = c("mal_falciparum", "ag_pct_gdp", "imports_percap"), int = NA, e = NA)
elast$var <- as.character(elast$var)

#mal_falciparum
m_mal_falciparum <- lm(mal_falciparum ~ gdp_pc, subset(log.data, mal_falciparum != is.na(mal_falciparum) & mal_falciparum > -Inf), na.action = na.omit); summary(m_mal_falciparum)
elast$int[elast$var == "mal_falciparum"] <- m_mal_falciparum$coefficients[["(Intercept)"]]
elast$e[elast$var == "mal_falciparum"] <- m_mal_falciparum$coefficients[["gdp_pc"]]

# #mal_vivax
# m_mal_vivax <- lm(mal_vivax ~ grid_gdp, subset(log.data, mal_vivax != is.na(mal_vivax) & mal_vivax > -Inf), na.action = na.omit); summary(m_mal_vivax)
# elast$int[elast$var == "mal_vivax"] <- m_mal_vivax$coefficients[["(Intercept)"]]
# elast$e[elast$var == "mal_vivax"] <- m_mal_vivax$coefficients[["grid_gdp"]]

# #market_dist
# m_market_dist <- lm(market_dist ~ grid_gdp, subset(log.data, market_dist != is.na(market_dist) & market_dist > -Inf), na.action = na.omit); summary(m_market_dist)
# elast$int[elast$var == "market_dist"] <- m_market_dist$coefficients[["(Intercept)"]]
# elast$e[elast$var == "market_dist"] <- m_market_dist$coefficients[["grid_gdp"]]

# #assistance
# m_assistance <- lm(assistance ~ grid_gdp, subset(log.data, assistance != is.na(assistance) & assistance > -Inf), na.action = na.omit); summary(m_assistance)
# elast$int[elast$var == "assistance"] <- m_assistance$coefficients[["(Intercept)"]]
# elast$e[elast$var == "assistance"] <- m_assistance$coefficients[["grid_gdp"]]

#ag_pct_gdp
m_ag_pct_gdp <- lm(ag_pct_gdp ~ gdp_pc, subset(log.data, ag_pct_gdp != is.na(ag_pct_gdp) & ag_pct_gdp > -Inf), na.action = na.omit); summary(m_ag_pct_gdp)
elast$int[elast$var == "ag_pct_gdp"] <- m_ag_pct_gdp$coefficients[["(Intercept)"]]
elast$e[elast$var == "ag_pct_gdp"] <- m_ag_pct_gdp$coefficients[["gdp_pc"]]

#imports_percap
m_imports_percap <- lm(imports_percap ~ gdp_pc, subset(log.data, imports_percap != is.na(imports_percap) & imports_percap > -Inf), na.action = na.omit); summary(m_imports_percap)
elast$int[elast$var == "imports_percap"] <- m_imports_percap$coefficients[["(Intercept)"]]
elast$e[elast$var == "imports_percap"] <- m_imports_percap$coefficients[["gdp_pc"]]

covar_trend <- gdp %>%
  select(year, iso3, gdp_pc) %>%
  mutate(gdp_pc = log(gdp_pc)) %>%
  filter(year %in% c(2020:2030))

covar_trend <- data.frame(covar_trend, matrix(nrow = nrow(covar_trend), ncol = 3))
names(covar_trend)[4:6] <- c("mal_falciparum", "ag_pct_gdp", "imports_percap")

covar_trend$mal_falciparum <- elast$int[elast$var == "mal_falciparum"] + elast$e[elast$var == "mal_falciparum"]*covar_trend$gdp_pc
# covar_trend$mal_vivax <- elast$int[elast$var == "mal_vivax"] + elast$e[elast$var == "mal_vivax"]*covar_trend$gdp_pc
# covar_trend$market_dist <- elast$int[elast$var == "market_dist"] + elast$e[elast$var == "market_dist"]*covar_trend$gdp_pc
# covar_trend$assistance <- elast$int[elast$var == "assistance"] + elast$e[elast$var == "assistance"]*covar_trend$gdp_pc
covar_trend$ag_pct_gdp <- elast$int[elast$var == "ag_pct_gdp"] + elast$e[elast$var == "ag_pct_gdp"]*covar_trend$gdp_pc
covar_trend$imports_percap <- elast$int[elast$var == "imports_percap"] + elast$e[elast$var == "imports_percap"]*covar_trend$gdp_pc

covar_trend[,3:6] <- exp(covar_trend[,3:6])
# covar_trend <- covar_trend %>% select(year, iso3, GDLCODE, v)

covar_trend <- covar_trend %>% select(iso3, year, mal_falciparum, ag_pct_gdp, imports_percap)
data <- data %>% select(iso3, year, mal_falciparum, ag_pct_gdp, imports_percap)

covar_trend <- rbind(data, covar_trend) %>% as.data.table

covar_trend <- covar_trend[order(year), .SD, by = iso3]

covar_trend <- covar_trend %>% filter(year %in% c(2010:2020, 2025, 2030)) 
gdp <- gdp %>% filter(year %in% c(2010:2020, 2025, 2030)) 


###-----------------------------------------------------------------------------------------------------------------
## static

ele_rug <- read.csv('data/covars_nowcast/results/elevation_ruggedness.csv')


###-----------------------------------------------------------------------------------------------------------------
## merge everything and put into cache

covar_fore <- Reduce(function(x, y){left_join(x, y)}, list(rur_urb, wc, gdp, hdi, gini, pc, iam, edu, covar_trend, ele_rug)) 

#some problem with Sibira
covar_fore$wci500[covar_fore$GDLCODE == "RUSr108"] <- NA
covar_fore$wci1000[covar_fore$GDLCODE == "RUSr108"] <- NA
covar_fore$wci1700[covar_fore$GDLCODE == "RUSr108"] <- NA
covar_fore$wci_index[covar_fore$GDLCODE == "RUSr108"] <- NA

cache('covar_fore')

