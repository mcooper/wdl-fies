dat <- fies_ur %>%
	select(region, fies.reg.mod.rur, fies.reg.mod.urb) %>%
	gather(key, val, -region) %>%
	mutate(key=ifelse(grepl('rur', key), 'Rural', 'Urban'))

ggplot(dat) +
	geom_bar(aes(x=region, y=val, fill=key), stat='identity', position='dodge') + 
	labs(y='Rate of Food Insecurity',
			 x='',
			 fill='') + 
	coord_flip() + 
	theme_bw() + 
	theme(legend.position=c(0.75, 0.25))

ggsave('figures/Urban_Rural.png', height=4, width=5)
