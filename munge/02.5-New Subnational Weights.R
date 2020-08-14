#Get new weights
#Urban-Rural
ur <- read.csv('data/covars/results/urban-rural.csv') %>%
  select(GDLCODE, YEAR, rural_perc, urban_perc)

#Read in WorldPop Demographic data
dem <- read.csv('data/disag_vars/results/age_gender_all1415.csv') %>%
  #determine percentages for every group over 15 & get total
  select(GDLCODE=GDLcode, YEAR=year, matches(paste0(seq(15, 80, by=5), collapse='|'))) %>%
  mutate(total = rowSums(select(., matches(paste0(seq(15, 80, by=5), collapse='|'))))) %>%
  mutate_at(vars(matches(paste0(seq(15, 80, by=5), collapse='|'))), function(x){x/.$total}) %>%
  select(-total)

#Read in DHS Wealth Data
dhs <- read.csv('data/disag_vars/results/wealth_quintiles.csv') %>%
  select(-ISO3)

#Recode fies vars to match ancillary data
fies_raw <- fies_raw %>%
  mutate(urb = case_when(Area == 'Urban/Suburbs' ~ 'urban',
                         Area == 'Towns/Rural' ~ 'rural',
                         TRUE ~ NA_character_),
         dhs = case_when(Income == 'Richest_20%' ~ 'wealth1',
                         Income == 'Second_20%' ~ 'wealth2',
                         Income == 'Middle_20%' ~ 'wealth3',
                         Income == 'Fourth_20%' ~ 'wealth4',
                         Income == 'Poorest_20%' ~ 'wealth5'),
         Age = floor(Age/5)*5,
         Age = ifelse(Age > 80, 80, Age),
         Gender = ifelse(Gender == 'Male', 'm', 'f'),
         dem = paste0(Gender, '_', Age)) %>%
  filter(!is.na(dem), !is.na(dhs), !is.na(urb),
         !is.na(Prob_Mod_Sev), !is.na(Prob_sev))


fies_subnat_wt <- data.frame()
for (year in c(2014, 2015)){
  for (gdlcode in unique(ur$GDLCODE)){
    print(paste(year, gdlcode))
    
    fies_sel <- fies_raw %>%
      filter(ISO3 == substr(gdlcode, 1, 3), 
             YEAR == year)
    
    if (nrow(fies_sel) == 0){
      next
    }
    
    pop_dfs <- list()
    smp_dfs <- list()
    
    #Urban-Rural probabilities
    pop_urb <- ur %>%
      filter(GDLCODE == gdlcode, 
             YEAR == year) %>%
      select(-GDLCODE, -YEAR) %>%
      gather(urb, urb.perc) %>%
      mutate(urb =substr(urb, 1, 5))
    
    smp_urb <- fies_sel %>%
      group_by(urb) %>%
      summarize(urb.perc=n()/nrow(.),
                .groups='drop')
    
    if (nrow(pop_urb) > 0){
      smp_dfs[[(length(smp_dfs) + 1)]] <- smp_urb
      pop_dfs[[(length(pop_dfs) + 1)]] <- pop_urb
    }
    
    #Demographic probabilities
    pop_dem <- dem %>%
      filter(GDLCODE == gdlcode,
             YEAR == year) %>%
      select(-GDLCODE, -YEAR) %>%
      gather(dem, dem.perc)
    smp_dem <- fies_sel %>%
      group_by(dem) %>%
      summarize(dem.perc = n()/nrow(.),
                .groups='drop')
    if (nrow(pop_dem) > 0){
      smp_dfs[[(length(smp_dfs) + 1)]] <- smp_dem
      pop_dfs[[(length(pop_dfs) + 1)]] <- pop_dem
    }
    
    #Wealth (DHS) Probabilities
    pop_dhs <- dhs %>%
      filter(GDLCODE == gdlcode) %>%
      select(-GDLCODE) %>%
      gather(dhs, dhs.perc)
    smp_dhs <- fies_sel %>%
      group_by(dhs) %>%
      summarize(dhs.perc = n()/nrow(.),
                .groups='drop')
    if (nrow(pop_dhs) > 0){
      smp_dfs[[(length(smp_dfs) + 1)]] <- smp_dhs
      pop_dfs[[(length(pop_dfs) + 1)]] <- pop_dhs
    }
    
    pop <- Reduce(merge, pop_dfs) %>%
      mutate(pop_prob = apply(select_if(., is.numeric), 1, prod)) %>%
      select(-matches('perc'))
    smp <- Reduce(merge, smp_dfs) %>%
      mutate(smp_prob = apply(select_if(., is.numeric), 1, prod)) %>%
      select(-matches('perc'))
    
    fies_sel_probs <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=F)},
                             list(fies_sel, pop, smp)) %>%
      mutate(samp_wt = pop_prob/smp_prob)
    
    new <- data.frame(YEAR=year,
                      GDLCODE=gdlcode,
                      fies.mod=weighted.mean(fies_sel_probs$Prob_Mod_Sev, w=fies_sel_probs$samp_wt),
                      fies.sev=weighted.mean(fies_sel_probs$Prob_sev, w=fies_sel_probs$samp_wt))
    
    fies_subnat_wt <- bind_rows(fies_subnat_wt, new)
  }
}

cache('fies_subnat_wt')
