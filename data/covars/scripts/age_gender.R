##------------------------------------------------------------------------------------------------------------------------------------

#from Max: change IP constantly to make unlimited amount of requests on worldpop api

#wait untill VPN is connected
mullvad_wait <- function(){
  connected <- FALSE
  start_time <- Sys.time()

  #wait While mullvad status is not connected
  while(connected == FALSE){

    # if loop runs longer than 2 minutes connect to other IP
    if (difftime(Sys.time(),start_time,units="mins") > 2){

      system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
      start_time <- Sys.time()
    }

    #Message and waiting
    message("Changing to different IP")
    nytnyt(periods=c(0,1))

    #Getting status of mullvad
    connected <- grepl("Connected to",system("mullvad status",intern=TRUE))


  }
}

#wait random amount of time
nytnyt <- function (periods = c(1,2)){
  # draw from a uniform distribution a single number between params
  tictoc <- runif(1, periods[1], periods[2])

  # Use a nice verbose output to communicate your intent
  cat(paste0(Sys.time()), "- Sleeping for ", round(tictoc, 2), "seconds\n")

  # Implement the sleeper
  Sys.sleep(tictoc)
}

library(readxl)
library(dplyr)
#read in list of mullvad servers for random IP adress switching
mullvad <- read_xlsx("data/covars/rawdata/age_gender/mullvad_countries.xlsx") %>%
  filter(! code %in% c("sg","jp"))

system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
system("mullvad status",intern=TRUE)
mullvad_wait()


##------------------------------------------------------------------------------------------------------------------------------------

#install.packages("devtools")
#devtools::install_github("wpgp/wpCPR")
library(wpCPR)
library(sf)

# sf <- st_read('GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')
sf <- st_read('data/gridded_gdlpolys', 'gridded_gdlpolys')


#sf_fies <- sf %>% filter(iso_code %in% unique(fies_raw$ISO3))
sf_fies <- sf %>% filter(GDLcode %in% unique(fail$GDLCODE))

sf_fies.list <- split(sf_fies, f = sf_fies$GDLcode)
sf_fies.list <- lapply(sf_fies.list, function(x) {for(i in 1:nrow(x)) {x$GDLcode_sub[i] <- paste0(x$GDLcode[i],"_",i)}; return(x)})
sf_fies <- do.call(rbind, sf_fies.list)

code <- unique(sf_fies$GDLcode_sub)

do.call(file.remove, list(list.files("data/GDL Shapefiles V4 FIES/", full.names = TRUE)))

#create shapefiles for all countries
for(c in code) {
  sf_fies_tmp <- subset(sf_fies, GDLcode_sub == c)
  sf_fies_tmp <- st_cast(sf_fies_tmp, "POLYGON")
  sf_fies_tmp <- st_make_valid(sf_fies_tmp)
  #plot(st_geometry(sf_fies_tmp))

  st_write(sf_fies_tmp, paste0("data/GDL Shapefiles V4 FIES/GDL Shapefiles V4_", c, ".shp"))
}

age_gender <- list()

# y <- 2014
# c <- "UKRr101"

for(y in 2014:2018) {
  i <- 1
  for(c in code) {
    try(age_gender[[paste0(y,"_",c)]] <-
          wpCPRDemographic(year = y,
                           shapeFilePath = paste0("data/GDL Shapefiles V4 FIES/GDL Shapefiles V4_",c,".shp"),
                           maxexectime = 3600,
                           verbose = T)
        )

    i <- i+1

    #change IP
    if(i%%25 == 0) {
      system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
      mullvad_wait() #wait while connecting
      nytnyt(periods=c(1,5))
      #print(paste0(y,": ",i,"/",length(code)))
    }
  }
}

ag_save <- age_gender
# age_gender <- ag_save

age_gender <- do.call(rbind, age_gender)

age_gender[,8:43] <- apply(age_gender[,8:43], 2, as.numeric)
ag_vars <- colnames(age_gender[,8:43])

# library(data.table)
# age_gender <- data.table(age_gender)
# age_gender[,.(shdi = shdi[1], apply(age_gender[,8:43],2,sum)), by = .(GDLcode, constant, iso_code, country, region, year)]

age_gender <- age_gender %>%
  group_by(GDLcode, constant, iso_code, country, region, shdi, year) %>%
  summarise_at(.funs = sum, .vars = ag_vars)

#write.csv(age_gender, 'data/covars/results/age_gender_missing.csv', row.names=F)
#write.csv(age_gender, 'data/covars/results/age_gender_toolarge.csv', row.names=F)

age_gender1 <- read.csv('data/covars/results/age_gender_missing.csv')
age_gender2 <- read.csv('data/covars/results/age_gender_toolarge.csv')

age_gender <- rbind(age_gender1, age_gender2)
age_gender <- age_gender[order(age_gender$GDLcode),]
#write.csv(age_gender, 'data/covars/results/age_gender_all.csv', row.names=F)

##------------------------------------------------------------------------------------------------------------------------------------

library(wpCPR)
library(sf)

age_gender <- read.csv('data/covars/results/age_gender_all.csv')

#plot map of countries that are not working
gdl <- sf_fies
mapdat <- gdl %>% 
  select(-region) %>%
  filter(iso_code %in% unique(fies_raw$ISO3))

countries <- ne_countries(returnclass='sf')
ggplot() + 
  geom_sf(data = mapdat %>% filter(iso_code %in% unique(fies_raw$ISO3)), color="black") +
  #geom_sf(data = mapdat %>% filter(GDLCODE == "IRLr101"), fill="red") +
  geom_sf(data = mapdat %>% filter(!GDLCODE %in% unique(age_gender$GDLcode)), fill="red") +
  geom_sf(data = mapdat %>% filter(GDLCODE %in% unique(age_gender$GDLcode)), fill="green") +
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void()

fail <- mapdat %>% filter(!GDLCODE %in% unique(age_gender$GDLcode))

#try again
age_gender_fail <- list()

#c("KHMr116", "ITAr121")
#c("RUSr102")
#c("AFGr104_1", "AFGr104_2", "AFGr104_3", "AFGr104_4", "AFGr104_5")
#fail$GDLCODE
file.remove("data/error_age_gender.txt")
errorFile <- file("data/error_age_gender.txt")

for(y in 2014:2018) {
  i <- 1
  for(c in code) {
    tryCatch({age_gender_fail[[paste0(y,"_",c)]] <- 
          wpCPRDemographic(year = y, 
                           shapeFilePath = paste0("data/GDL Shapefiles V4 FIES/GDL Shapefiles V4_",c,".shp"), 
                           maxexectime = 3600, 
                           verbose = T)
    }, error = function(e) {
      cat(paste0(c, " in ", y, ":  ", as.character(e)), file = "data/error_age_gender.txt", append = TRUE)
    })
    
    i <- i+1
    
    #change IP
    if(i%%50 == 0) {
      system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
      mullvad_wait() #wait while connecting
      nytnyt(periods=c(1,5))
    }
  }
}
close(errorFile)
save.image(file = "data/ag_save1.RData", version = NULL, ascii = FALSE, compress = !ascii, safe = TRUE)

age_gender_fail <- do.call(rbind, age_gender_fail)

age_gender_fail[,9:44] <- apply(age_gender_fail[,9:44], 2, as.numeric)
ag_vars_fail <- colnames(age_gender_fail[,9:44])

age_gender_fail <- age_gender_fail %>%
  filter(year %in% c(2014, 2015)) %>%
  group_by(GDLcode, constnt, iso_cod, country, region, shdi, GDLcd_s, year) %>%
  summarise_at(.funs = sum, .vars = ag_vars_fail)

#write.csv(age_gender_fail, 'data/covars/results/age_gender_subdivi1415.csv', row.names=F)






age_gender <- read.csv('data/covars/results/age_gender_withoutsubdivi.csv')
age_gender_fail <- read.csv('data/covars/results/age_gender_subdivi1415.csv') %>% filter(year == 2015)

#plot map
mapdat <- gdl %>% 
  select(-region) %>%
  filter(iso_code %in% unique(fies_raw$ISO3))

mapdat_fail <- sf_fies %>% 
  select(-region) %>%
  filter(iso_code %in% unique(fies_raw$ISO3))

countries <- ne_countries(returnclass='sf')
ggplot() + 
  geom_sf(data = mapdat %>% filter(iso_code %in% unique(fies_raw$ISO3)), color="black") +
  #geom_sf(data = mapdat %>% filter(GDLCODE == "IRLr101"), fill="red") +
  geom_sf(data = mapdat %>% filter(!GDLCODE %in% unique(age_gender$GDLcode)), fill="red") +
  geom_sf(data = mapdat %>% filter(GDLCODE %in% unique(age_gender$GDLcode)), fill="green") +
  
  geom_sf(data = mapdat_fail %>% filter(!GDLcode_sub %in% unique(age_gender_fail$GDLcd_s)), fill="red") +
  geom_sf(data = mapdat_fail %>% filter(GDLcode_sub %in% unique(age_gender_fail$GDLcd_s)), fill="green") +
  
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void()


age_gender_fail <- read.csv('data/covars/results/age_gender_subdivi1415.csv') 

age_gender_fail <- age_gender_fail %>%
  group_by(GDLcode, constnt, iso_cod, country, region, shdi, year) %>%
  summarise_at(.funs = sum, .vars = ag_vars_fail) %>%
  rename(constant = "constnt", iso_code = "iso_cod")

age_gender_1415 <- rbind(age_gender, age_gender_fail)
age_gender_1415 <- age_gender_1415 %>% filter(year %in% c(2014, 2015))
age_gender_1415 <- age_gender_1415[order(age_gender_1415$GDLcode),]

write.csv(age_gender_1415, 'data/covars/results/age_gender_all1415.csv', row.names=F)

# #check pop/age/gender for austria
# age_gender$total <- apply(age_gender[,8:43], 1, sum) #check if data makes somehow sense
# library(data.table)
# iso_total <- data.table(age_gender %>% select(GDLcode, year, total))
# iso_total <- iso_total[,.(iso_total = sum(.SD$total)), by = .(iso_code, year)] #seems to be ok, checked austria
# 
# #what should we do?
# #it seems that this package works fine for 46 countries (31 missing)
# #using the global age gender 1km res maps implies that we need to process 3.05gb files for both m/f and all age groups and all years
# #not possible on my computer and i think also quite a demanding exercise for a cloud based approach





