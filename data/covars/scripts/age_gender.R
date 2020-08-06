# 
# 
# ##------------------------------------------------------------------------------------------------------------------------------------
# 
# #from Max: change IP constantly to make unlimited amount of requests on worldpop api
# 
# #wait untill VPN is connected
# mullvad_wait <- function(){
#   connected <- FALSE
#   start_time <- Sys.time()
# 
#   #wait While mullvad status is not connected
#   while(connected == FALSE){
# 
#     # if loop runs longer than 2 minutes connect to other IP
#     if (difftime(Sys.time(),start_time,units="mins") > 2){
# 
#       system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
#       start_time <- Sys.time()
#     }
# 
#     #Message and waiting
#     message("Changing to different IP")
#     nytnyt(periods=c(0,1))
# 
#     #Getting status of mullvad
#     connected <- grepl("Connected to",system("mullvad status",intern=TRUE))
# 
# 
#   }
# }
# 
# #wait random amount of time
# nytnyt <- function (periods = c(1,2)){
#   # draw from a uniform distribution a single number between params
#   tictoc <- runif(1, periods[1], periods[2])
# 
#   # Use a nice verbose output to communicate your intent
#   cat(paste0(Sys.time()), "- Sleeping for ", round(tictoc, 2), "seconds\n")
# 
#   # Implement the sleeper
#   Sys.sleep(tictoc)
# }
# 
# library(readxl)
# library(dplyr)
# #read in list of mullvad servers for random IP adress switching
# mullvad <- read_xlsx("data/covars/rawdata/age_gender/mullvad_countries.xlsx") %>%
#   filter(! code %in% c("sg","jp"))
# 
# system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
# system("mullvad status",intern=TRUE)
# mullvad_wait()
# 
# #install.packages("devtools")
# #devtools::install_github("wpgp/wpCPR")
# library(wpCPR)
# library(sf)
# 
# sf <- st_read('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')
# 
# sf_fies <- sf %>% filter(iso_code %in% unique(fies_raw$ISO3))
# #sf_fies <- sf %>% filter(GDLcode == "AUTr108")
# 
# code <- unique(sf_fies$GDLcode)
# 
# 
# do.call(file.remove, list(list.files("data/GDL Shapefiles V4 FIES/", full.names = TRUE)))
# 
# #create shapefiles for all countries
# for(c in code) {
#   sf_fies_tmp <- subset(sf_fies, GDLcode == c)
#   sf_fies_tmp <- st_cast(sf_fies_tmp, "POLYGON")
#   sf_fies_tmp <- st_make_valid(sf_fies_tmp)
#   #plot(st_geometry(sf_fies_tmp))
# 
#   st_write(sf_fies_tmp, paste0("data/GDL Shapefiles V4 FIES/GDL Shapefiles V4_", c, ".shp"))
# }
# 
# age_gender <- list()
# fail <- NA
# 
# y <- 2014
# c <- "RUSr101"
# 
# for(y in 2014:2018) {
#   i <- 1
#   for(c in code) {
#     tryCatch({
#       age_gender[[paste0(y,"_",c)]] <- wpCPRDemographic(year = y, shapeFilePath = paste0("data/GDL Shapefiles V4 FIES/GDL Shapefiles V4_",c,".shp"), maxexectime = 3600, verbose = T)
#     }, error = function(e) {
#       age_gender[[paste0(y,"_",c)]] <- NA; print(paste0(c, "failed"))
#     })
#     
#     i <- i+1
#     
#     #change IP
#     if(i%%50 == 0) {
#       system(paste("mullvad relay set location",sample(mullvad$code,1), sep=" "))
#       mullvad_wait() #wait while connecting
#       nytnyt(periods=c(1,5))
#       print(paste0(y,": ",i,"/",length(code)))
#     }
#   }
# }
# 
# # ag_save <- age_gender
# # age_gender <- ag_save
# 
# age_gender <- do.call(rbind, age_gender)
# 
# age_gender[,8:43] <- apply(age_gender[,8:43], 2, as.numeric)
# ag_vars <- colnames(age_gender[,8:43])
# 
# # library(data.table)
# # age_gender <- data.table(age_gender)
# # age_gender[,.(shdi = shdi[1], apply(age_gender[,8:43],2,sum)), by = .(GDLcode, constant, iso_code, country, region, year)]
# 
# library(dplyr)
# age_gender <- age_gender %>%
#   group_by(GDLcode, constant, iso_code, country, region, shdi, year) %>%
#   summarise_at(.funs = sum, .vars = ag_vars)
# 
# #write.csv(age_gender, 'data/covars/results/age_gender_31missing.csv', row.names=F)

##------------------------------------------------------------------------------------------------------------------------------------

age_gender <- read.csv('data/covars/results/age_gender_missing.csv')

length(setdiff(unique(fies_raw$ISO3), unique(age_gender$iso_code))) #we are missing 31/77 countries, shit!!
fail <- setdiff(unique(fies_raw$ISO3), unique(age_gender$iso_code))


#plot map of countries that are not working
mapdat <- gdl %>% 
  select(-region) %>%
  filter(iso_code %in% unique(fies_raw$ISO3))

countries <- ne_countries(returnclass='sf')
ggplot() + 
  geom_sf(data = mapdat %>% filter(iso_code %in% unique(fies_raw$ISO3)), color="black") +
  geom_sf(data = mapdat %>% filter(!iso_code %in% unique(age_gender$GDLcode)), fill="red") +
  geom_sf(data = mapdat %>% filter(GDLCODE %in% unique(age_gender$GDLcode)), fill="green") +
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void()


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











