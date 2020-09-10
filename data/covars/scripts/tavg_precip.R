library(tidyverse)
library(raster)
library(rgdal)
library(gdalUtils)
library(foreach)
library(doParallel)

summarizeYear <- function(path){
  s <- stack(path)
  indices <- as.numeric(format(as.Date(names(s), format = "X%Y.%m.%d"), format = "%Y"))
  year_sum <- stackApply(s, indices, fun=mean, na.rm=T)
  year_sum
}

writeStack <- function(x, path){
  for (n in names(x)){
    s <- x[[n]]
    writeRaster(s, paste0('proc/',
                          path, '_', n, '.tif'),
                format='GTiff')

  }
}

start <- Sys.time()
extract(s, matrix(c(15, 15, 16, 16), ncol=2))
end <- Sys.time()

start - end

#######################################
# Process future data from Ensembles
######################################
setwd('~/mortalityblob/scenarios/rcp60')

mod1_2020s <- summarizeYear('GFDL-ESM2M/tas_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod1_2020s, 'mod1_tave')

mod2_2020s <- summarizeYear('HadGEM2-ES/tas_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod2_2020s, 'mod2_tave')

mod3_2020s <- summarizeYear('IPSL-CM5A-LR/tas_day_IPSL-CM5A-LR_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod3_2020s, 'mod3_tave')

mod4_2020s <- summarizeYear('MIROC5/tas_day_MIROC5_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod4_2020s, 'mod4_tave')

#######################################
# Process future data from Ensembles
######################################
mod1_2020s <- summarizeYear('GFDL-ESM2M/pr_day_GFDL-ESM2M_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod1_2020s, 'mod1_precip')

mod2_2020s <- summarizeYear('HadGEM2-ES/pr_day_HadGEM2-ES_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod2_2020s, 'mod2_precip')

mod3_2020s <- summarizeYear('IPSL-CM5A-LR/pr_day_IPSL-CM5A-LR_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod3_2020s, 'mod3_precip')

mod4_2020s <- summarizeYear('MIROC5/pr_day_MIROC5_rcp60_r1i1p1_EWEMBI_landonly_20210101-20301231.nc4')
writeStack(mod4_2020s, 'mod4_precip')

system('~/telegram.sh "Done converting stacks to tiffs"')


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
sp <- readOGR('~/wdl-fies/data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4 Edit')

p <- data.frame()

for (i in 1:nrow(sp)){
  res <- data.frame(spsample(sp[i, ], n=50, 'random'))
  res$GDLCODE <- sp@data$GDLcode[i]
  p <- bind_rows(p, res)
}

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
