#Read in GDL Code
gdl <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4') %>%
  rename(GDLCODE=GDLcode) %>%
  filter(!GDLCODE %in% c("COLr128", "GMBr101", "GMBr107", "INDr135", "NA"))

#Read in Pop data from Covariates
#Subset to areas we have GDL codes for
pop <- read.csv('data/covars/results/urban-rural.csv')

pop_cty <- pop %>%
  mutate(Rural = population*rural_perc,
         Urban = population*urban_perc) %>%
  group_by(ISO3, YEAR) %>%
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
                              ISO3, YEAR),
                     pop %>%
                      mutate(Rural = population*rural_perc,
                             Urban = population*urban_perc),
                     all.x=T, all.y=F) %>%
  mutate(fies.mod = (fies.mod.rur*Rural + fies.mod.urb*Urban)/(Rural + Urban),
         fies.sev = (fies.sev.rur*Rural + fies.sev.urb*Urban)/(Rural + Urban))

cache('fies_subnat')
cache('gdl')

