library(raster)
library(rgdal)
library(gdalUtils)
library(foreach)
library(doParallel)
library(tidyverse)

sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

p <- data.frame()

for (i in 1:nrow(sp)){
  res <- data.frame(spsample(sp[i, ], n=50, 'random'))
  res$GDLCODE <- sp@data$GDLcode[i]
  p <- bind_rows(p, res)
}

#######################################
# Process future tave data from Ensembles
######################################
setwd('~/mortalityblob/scenarios/rcp60')

mod1_2020s <- stack('GFDL-ESM2M/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod1_2020s, p %>% dplyr::select(x, y))
mod1_tave <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod1_tave=mean(value, na.rm=T))
write.csv(mod1_tave, '~/wdl-fies/data/covars/rawdata/mod1_tave.csv', row.names=F)
system('~/telegram.sh "Done with mod1_tave"')

mod2_2020s <- stack('HadGEM2-ES/tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod2_2020s, p %>% dplyr::select(x, y))
mod2_tave <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod2_tave=mean(value, na.rm=T))
write.csv(mod2_tave, '~/wdl-fies/data/covars/rawdata/mod2_tave.csv', row.names=F)
system('~/telegram.sh "Done with mod2_tave"')

mod3_2020s <- stack('IPSL-CM5A-LR/tas_day_IPSL-CM5A-LR_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod3_2020s, p %>% dplyr::select(x, y))
mod3_tave <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod3_tave=mean(value, na.rm=T))
write.csv(mod3_tave, '~/wdl-fies/data/covars/rawdata/mod3_tave.csv', row.names=F)
system('~/telegram.sh "Done with mod3_tave"')

mod4_2020s <- stack('MIROC5/tas_day_MIROC5_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod4_2020s, p %>% dplyr::select(x, y))
mod4_tave <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod4_tave=mean(value, na.rm=T))
write.csv(mod4_tave, '~/wdl-fies/data/covars/rawdata/mod4_tave.csv', row.names=F)
system('~/telegram.sh "Done with mod4_tave"')

#######################################
# Process future precip data from Ensembles
######################################
mod1_2020s <- stack('GFDL-ESM2M/pr_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod1_2020s, p %>% dplyr::select(x, y))
mod1_precip <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod1_precip=mean(value, na.rm=T))
write.csv(mod1_precip, '~/wdl-fies/data/covars/rawdata/mod1_precip.csv', row.names=F)
system('~/telegram.sh "Done with mod1_precip"')

mod2_2020s <- stack('HadGEM2-ES/pr_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod2_2020s, p %>% dplyr::select(x, y))
mod2_precip <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod2_precip=mean(value, na.rm=T))
write.csv(mod2_precip, '~/wdl-fies/data/covars/rawdata/mod2_precip.csv', row.names=F)
system('~/telegram.sh "Done with mod2_precip"')

mod3_2020s <- stack('IPSL-CM5A-LR/pr_day_IPSL-CM5A-LR_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod3_2020s, p %>% dplyr::select(x, y))
mod3_precip <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod3_precip=mean(value, na.rm=T))
write.csv(mod3_precip, '~/wdl-fies/data/covars/rawdata/mod3_precip.csv', row.names=F)
system('~/telegram.sh "Done with mod3_precip"')

mod4_2020s <- stack('MIROC5/pr_day_MIROC5_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
res <- raster::extract(mod4_2020s, p %>% dplyr::select(x, y))
mod4_precip <- cbind(p, res) %>%
  gather(year, value, -x, -y, -GDLCODE) %>%
  mutate(YEAR = as.numeric(substr(year, 2, 5))) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(mod4_precip=mean(value, na.rm=T))
write.csv(mod4_precip, '~/wdl-fies/data/covars/rawdata/mod4_precip.csv', row.names=F)
system('~/telegram.sh "Done with mod4_precip"')

#################################
# Combine and Clean Future Data
####################################
fut <- Reduce(merge, list(mod1_tave, mod2_tave, mod3_tave, mod4_tave,
                          mod1_precip, mod2_precip, mod3_precip, mod4_precip)) %>%
  mutate(tave = (mod1_tave + mod2_tave + mod3_tave + mod4_tave)/4 - 273.15,
         precip = ((mod1_precip + mod2_precip + mod3_precip + mod4_precip)/4)*365) %>%
  select(-matches('mod'))

write.csv(fut, '~/wdl-fies/data/covars/rawdata/temp-precip-future.csv', row.names=F) 

###############################
# Move data to mount drive
##############################
# sudo mkdir /mnt/tiffs
# sudo chown mattcoop /mnt/tiffs
# cp ~/mortalityblob/TerraClimate/TerraClimate_ppt_201* /mnt/tiffs
# cp ~/mortalityblob/TerraClimate/TerraClimate_tmax_201* /mnt/tiffs
# cp ~/mortalityblob/TerraClimate/TerraClimate_tmin_201* /mnt/tiffs

##########################################
# Get Historic Data From TerraClimate
##########################################
setwd('/mnt/tiffs')

#Read in historic precip data
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(pattern='TerraClimate_ppt.*tif')
gdalbuildvrt(precip_files, precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in historic tmax data
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(pattern='TerraClimate_tmax.*tif')
gdalbuildvrt(tmax_files, tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in historic tmin data
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(pattern='TerraClimate_tmin.*tif')
gdalbuildvrt(tmin_files, tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(48, outfile = '')
registerDoParallel(cl)

extract_info <- function(vrt, x, y){
  res <- as.numeric(gdallocationinfo(vrt, x=x, y=y, wgs84=TRUE, valonly=TRUE))

  res[res > 99999 | res < -99999] <- NA

  res
}

df <- foreach(n=1:nrow(p), .packages=c('raster', 'gdalUtils', 'tidyverse'), .combine=bind_rows) %dopar% {
  pastprecip <- extract_info(precip_vrt_file, x=p$x[n], y=p$y[n])
  pasttmin <- extract_info(tmin_vrt_file, x=p$x[n], y=p$y[n])
  pasttmax <- extract_info(tmax_vrt_file, x=p$x[n], y=p$y[n])

  pasttemp <- (pasttmin + pasttmax)/2

  pastdf <- data.frame(year=rep(2010:2018, each=12),
                       temp=pasttemp,
                       precip=pastprecip) %>%
    group_by(year) %>%
    summarize(temp=mean(temp),
              precip=sum(precip)) %>%
    mutate(GDLCODE = p$GDLCODE[n])

  cat(n, round(n/nrow(p)*100, 4), 'percent done\n')

  pastdf
}

sum <- df %>%
  rename(YEAR=year) %>%
  group_by(GDLCODE, YEAR) %>%
  summarize(precip=mean(precip, na.rm=T),
            temp=mean(temp, na.rm=T))

write.csv(sum, '~/wdl-fies/data/covars/rawdata/temp-precip-past.csv', row.names=F)

###########################################
# Harmonize Past & Future
##########################################

fut <- read.csv('~/wdl-fies/data/covars/rawdata/temp-precip-future.csv') %>%
  mutate(precip=precip*100000)
pas <- read.csv('~/wdl-fies/data/covars/rawdata/temp-precip-past.csv') %>%
  rename(tave=temp)

#Fill missing years, get slopes and predict values from linear trend
all <- bind_rows(fut, pas) %>%
  arrange(GDLCODE, YEAR) %>%
  complete(GDLCODE, YEAR=2010:2030) %>%
  group_by(GDLCODE) %>%
  nest %>%
  mutate(pint=map(data, function(df) lm(precip~YEAR, data=df)$coefficients[1]),
         tint=map(data, function(df) lm(tave~YEAR, data=df)$coefficients[1]),
         pcoef=map(data, function(df) lm(precip~YEAR, data=df)$coefficients[2]),
         tcoef=map(data, function(df) lm(tave~YEAR, data=df)$coefficients[2])) %>%
  unnest(pint, tint, pcoef, tcoef) %>%
  unnest(data) %>%
  mutate(ppred = pint + pcoef*YEAR,
         tpred = tint + tcoef*YEAR,
         precip = ifelse(is.na(precip), ppred, precip),
         tave = ifelse(is.na(tave), tpred, tave))

write.csv(all %>% select(GDLCODE, YEAR, tave, precip),
          '~/wdl-fies/data/covars/results/tave-precip.csv', row.names=F)

# library(sf)
# sf <- st_as_sf(sp)
# 
# m <- merge(sf, all %>% select(GDLcode=GDLCODE, pcoef, tcoef) %>% unique)
# 
# ggplot() + 
#   geom_sf(data=m, aes(fill=tcoef)) + 
#   scale_fill_gradient2(low=ihme_cols[2], mid='white', high=ihme_cols[9])
# 
# ggplot() + 
#   geom_sf(data=m, aes(fill=pcoef)) + 
#   scale_fill_gradient2(low=ihme_cols[2], mid='white', high=ihme_cols[9])
# 
# m2 <- merge(sf, all %>% rename(GDLcode=GDLCODE) %>% filter(YEAR %in% c(2010, 2020, 2030)))
# 
# #2010
# ggplot() + 
#   geom_sf(data=m2 %>% filter(YEAR == 2010),
#           aes(fill=tave)) + 
#   scale_fill_gradientn(colors=ihme_cols)
# 
# #2020
# ggplot() + 
#   geom_sf(data=m2 %>% filter(YEAR == 2020),
#           aes(fill=tpred)) + 
#   scale_fill_gradientn(colors=ihme_cols)
# 
# #2030
# ggplot() + 
#   geom_sf(data=m2 %>% filter(YEAR == 2030),
#           aes(fill=precip)) + 
#   scale_fill_gradientn(colors=ihme_cols)
