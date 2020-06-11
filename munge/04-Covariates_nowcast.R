fs <- list.files('data/nowcast/results/', full.names=T)

start <- 'data/nowcast/results/gdl_vars.csv'

fs <- fs[fs != start]

covar_now <- read.csv(start)








##############################
## covariates nowcast model ##
##############################

###-----------------------------------------------------------------------------------------------------------------
## extracted covar

## predict some of the extracted covariates with simple model
#all covariates extracted in the first round on the 22.04.2020
covar_now <- read.csv('data/covars_nowcast/GDL_extract_2020_04_22.csv')

#additional covar extracted on the 29.04.2020
livestock <- read.csv('data/covars_nowcast/GDL_livestock_2020_04_29.csv')
malaria <- read.csv('data/covars_nowcast/GDL_malaria_2020_04_29.csv')
#spei <- read.csv('data/covars_nowcast/GDL_spei_2020_04_29.csv')


#manipulation
covar_now$GDLCode <- substr(covar_now$GDLCode, 42, 100)
covar_now <- reshape2::dcast(covar_now, GDLCode + year ~ variable)
covar_now <- covar_now %>% dplyr::select(-c(bodycount, nutritiondiversity))

#livestock
colnames(livestock) <- c("GDLCode", "buffaloes", "cattle", "chickens", "ducks", "horses", "pigs", "sheep")
covar_now <- merge(covar_now, livestock, by = "GDLCode", all.x=T, all.y=F)

#malaria
malaria <- malaria %>% rename(mal_falciparum = "falciparum", mal_vivax = "vivax")
covar_now <- merge(covar_now, malaria, by = c("GDLCode", "year"), all.x=T, all.y=F)

# #Dont deal with SPEI right now
# #spei
# spei$year <- year(ymd(gsub('X', '', spei$date)))
# spei$month <- month(ymd(gsub('X', '', spei$date)))
# spei <- spei %>% select(-c(date))
# 
# covar_now <- data.frame(covar_now[rep(1:nrow(covar_now), 12), ], month = rep(1:12, each = nrow(covar_now)))
# covar_now <- merge(covar_now, spei, by = c("GDLCode", "year", "month"), all.x=T, all.y=F)

covar_now <- covar_now %>% 
	rename(GDLCODE = "GDLCode") %>%
	mutate(iso3 = substr(GDLCODE, 1, 3))

#Places with missing malaria data just have no malaria
covar_now$mal_falciparum[is.na(covar_now$mal_falciparum)] <- 0
covar_now$mal_vivax[is.na(covar_now$mal_vivax)] <- 0

#For missing data first pad NAs with most recent available year
covar_now <- covar_now %>% 
		#Make NANs NA
		#Fill in missing years with nearest chronological year
		arrange(GDLCODE, year) %>%
		group_by(GDLCODE) %>%
		mutate_each(funs(na.locf(na.locf(., na.rm=F), na.rm=F, fromLast=T))) %>%
		group_by(iso3, year) %>%
		mutate_each(funs(replace(., which(is.na(.)), mean(., na.rm=TRUE))))

#countrycode(covar_now$iso3[apply(covar_now, MARGIN=1, FUN=function(x){any(is.na(x))})] %>% unique, 'iso3c', 'country.name')
#All other missing data is islands or North Latitudes


cache('covar_now')

