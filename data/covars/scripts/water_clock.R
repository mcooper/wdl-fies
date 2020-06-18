library(data.table)

#water scarcity
fs <- list.files('data/covars/rawdata/wc', pattern='csv$', recursive = T, full.names = T)
wc_list <- list()
i <- 1

for (f in fs){
  n <- gsub('data/covars/rawdata/wc/','', f)
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

wc <- wc %>% 
  select(GDLCODE, YEAR=year, wci_index)

write.csv(wc, 'data/covars/results/water_clock.csv', row.names=F)
