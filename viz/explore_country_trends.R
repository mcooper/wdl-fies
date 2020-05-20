fies_trends <- merge(
  fies_cty %>%
    group_by(iso3) %>%
    do(tidy(lm(fies.mod ~ year, .))) %>% 
    filter(!is.nan(estimate),
           term == 'year') %>%
    select(iso3, mod.coef=estimate, mod.pval=p.value),
  fies_cty %>%
    group_by(iso3) %>%
    do(tidy(lm(fies.sev ~ year, .))) %>% 
    filter(!is.nan(estimate),
           term == 'year') %>%
    select(iso3, sev.coef=estimate, sev.pval=p.value)
  )

country_map(fies_trends, 'iso3', 'mod.coef')
country_map(fies_trends, 'iso3', 'sev.coef')
