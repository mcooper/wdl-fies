#Read in GDL Code
gdl <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4') %>%
  rename(GDLCODE=GDLcode)

#Read in Pop data from Covariates
#Subset to areas we have GDL codes for
pop <- read.csv('data/covars/SSP2_Vars_Historic.csv') %>%
  select(year=Year, iso3=iso3c, Rural, Urban, GDLCODE=GDLcode) %>%
  filter(GDLCODE %in% gdl$GDLCODE, 
         !is.na(iso3), !is.na(GDLCODE))

pop_cty <- pop %>%
  group_by(iso3, year) %>%
  summarize(Rural.Pop = sum(Rural),
            Urban.Pop = sum(Urban),
            Total.Pop = Rural.Pop + Urban.Pop)

fies_ur_pop_cty <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)}, 
                      list(fies_cty, regions, fies_ur, pop_cty)) %>%
  select(-matches('3yr')) %>%
  mutate(fies.mod.rur = (fies.mod*Total.Pop)/(ur.ratio.mod*Urban.Pop + Rural.Pop),
         fies.mod.urb = ur.ratio.mod*fies.mod.rur,
         fies.sev.rur = (fies.sev*Total.Pop)/(ur.ratio.sev*Urban.Pop + Rural.Pop),
         fies.sev.urb = ur.ratio.sev*fies.sev.rur)

fies_subnat <- merge(fies_ur_pop_cty %>%
                       select(fies.mod.rur, fies.mod.urb, fies.sev.rur, fies.sev.urb,
                              iso3, year),
                     pop, 
                     all.x=T, all.y=F) %>%
  mutate(fies.mod = (fies.mod.rur*Rural + fies.mod.urb*Urban)/(Rural + Urban),
         fies.sev = (fies.sev.rur*Rural + fies.sev.urb*Urban)/(Rural + Urban))

cache('fies_subnat')
cache('gdl')
