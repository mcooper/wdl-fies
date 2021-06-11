setwd('~/wdl-fies');library(ProjectTemplate);load.project()

dhs <- read_sf('~/wdl-fies/data/disag_vars/rawdata/dhs_statcompiler/shps', 
               'sdr_subnational_data_dhs_2015')

fies_subnat$iso2 <- countrycode(substr(fies_subnat$GDLCODE, 1, 3), 'iso3c', 'iso2c')

int <- intersect(fies_subnat$iso2, dhs$ISO)

fies_sel <- fies_subnat %>%
  filter(iso2 %in% int) %>%
  group_by(GDLCODE, iso2) %>%
  summarize(fies.mod = mean(fies.mod),
            fies.sev = mean(fies.sev))

fies_sel <- merge(gdl, fies_sel)

dhs_sel <- dhs %>%
  filter(ISO %in% int) %>%
  select(dhs.stnt=CNNUTSCHA2, dhs.wast=CNNUTSCWH2, ISO)

for (i in int){
  
  ggplot(fies_sel %>% filter(iso2 == i)) + 
    geom_sf(aes(fill=fies.sev)) + 
    scale_fill_gradient(low="#fef0d9", high="#b30000") + 
    theme_void()
  ggsave(paste0('~/wdl-fies/figures/compare_dhs/', i, '_fies.sev.png'))

  ggplot(fies_sel %>% filter(iso2 == i)) + 
    geom_sf(aes(fill=fies.mod)) + 
    scale_fill_gradient(low="#fef0d9", high="#b30000") + 
    theme_void()
  ggsave(paste0('~/wdl-fies/figures/compare_dhs/', i, '_fies.mod.png'))

  ggplot(dhs_sel %>% filter(ISO == i)) + 
    geom_sf(aes(fill=dhs.stnt)) + 
    scale_fill_gradient(low="#fef0d9", high="#b30000") + 
    theme_void()
  ggsave(paste0('~/wdl-fies/figures/compare_dhs/', i, '_dhs.stnt.png'))

  ggplot(dhs_sel %>% filter(ISO == i)) + 
    geom_sf(aes(fill=dhs.wast)) + 
    scale_fill_gradient(low="#fef0d9", high="#b30000") + 
    theme_void()
  ggsave(paste0('~/wdl-fies/figures/compare_dhs/', i, '_dhs.wast.png'))

}

fies_sel_pt <- st_centroid(fies_sel)

comb <- st_join(fies_sel_pt, dhs_sel) %>%
  filter(!is.na(dhs.stnt))

cor(st_drop_geometry(comb)[ , c('fies.mod', 'fies.sev', 'dhs.stnt', 'dhs.wast')])

summary(lm(fies.sev ~ dhs.stnt + ISO, data=comb))
summary(lm(fies.mod ~ dhs.stnt + ISO, data=comb))
summary(lm(fies.sev ~ dhs.wast + ISO, data=comb))
summary(lm(fies.mod ~ dhs.wast + ISO, data=comb))

for (i in int){
  sel <- comb %>%
    filter(ISO == i)
  print(i)
  print(cor(sel$fies.mod, sel$dhs.stnt))
}






