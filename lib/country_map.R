country_map <- function(dat, iso3_col, var_col){
  #Make a quick map
  # data: A dataframe with the data
  # iso3_col: the column with the country codes, in iso3 format
  # var_col: a continuous variable to be mapped
  
  cty <- ne_countries(scale = 50, returnclass = 'sf')
  
  dat$iso_a3 <- dat[ , iso3_col]
  dat$var <- dat[ , var_col]

  if (sum(duplicated(dat$iso_a3)) > 0){
    dat <- dat %>%
      group_by(iso_a3) %>%
      summarize(var = mean(var, na.rm=T))
  }
  
  cty <- merge(cty, dat, all.x=T)
  
  plt <- ggplot(cty) + 
    geom_sf(aes(fill=var)) + 
    labs(fill=var_col) + 
    scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                   "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
    coord_sf(crs='+proj=robin') + 
    theme_void() + 
    theme(legend.position = 'bottom',
          plot.title = element_text(hjust = 0.5))
  
  return(plt)
}
