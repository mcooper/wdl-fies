setwd('~/wdl-fies'); library(ProjectTemplate); load.project()

preddatsum <- preddat %>%
  group_by(ISO3, YEAR) %>%
  summarize(fies.mod.pred = weighted.mean(fies.mod.pred*population, w=population),
            fies.sev.pred = weighted.mean(fies.sev.pred*population, w=population)) %>%
  group_by(ISO3) %>%
  mutate(fies.mod.pred = fies.mod.pred/lag(fies.mod.pred),
         fies.sev.pred = fies.sev.pred/lag(fies.sev.pred))

# Run PCA regressions
pcadatmod <- preddatsum %>%
  select(ISO3, YEAR, fies.mod.pred) %>%
  spread(YEAR, fies.mod.pred) %>%
  ungroup %>%
  select(-`2010`)

pcamod <- princomp(pcadatmod %>% select(-ISO3))

pcadatsev <- preddatsum %>%
  select(ISO3, YEAR, fies.sev.pred) %>%
  spread(YEAR, fies.sev.pred) %>%
  ungroup %>%
  select(-`2010`)

pcasev <- princomp(pcadatsev %>% select(-ISO3))

# Make Plot data
pcaplt <- cbind(pcadatsev %>% 
                     select(iso_a3=ISO3),
                   pcamod$scores[ , 1:4] %>%
                     data.frame %>%
                     rename(mod1=Comp.1, mod2=Comp.2, mod3=Comp.3, mod4=Comp.4),
                   pcasev$scores[ , 1:4] %>%
                     data.frame %>%
                     rename(sev1=Comp.1, sev2=Comp.2, sev3=Comp.3, sev4=Comp.4))

# Plot
world <- ne_countries(returnclass='sf')

m <- merge(world %>%
             select(iso_a3),
           pcaplt)

ggplot() + 
  geom_sf(data=m, aes(fill=mod1)) + 
  scale_fill_gradientn(colors=ihme_cols)
