# setwd('~/wdl-fies'); library(ProjectTemplate); load.project(list(data_loading=T))

setwd('~/wdl-fies/docs/img')
setwd('./docs/img')

options(scipen=100)

#####################
# totals time series
###############

#get totals by YEAR
totals <- preddat %>%
  filter(YEAR >= 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.total=sum(fies.mod.pred * (population), na.rm=t),
            sev.total=sum(fies.sev.pred * (population), na.rm=t)) %>%
  gather(var, value, -YEAR) %>%
  mutate(var = ifelse(grepl('mod', var), "Moderate And Severe", "Severe")) %>%
  group_by(var) %>%
  mutate(value = rollapply(value, width=3, FUN=mean, align='center', partial=TRUE))

ggplot(totals) + 
  geom_line(aes(x=YEAR, y=value, linetype=var), size=1.2) + 
  #scale_color_manual(values=c("Moderate And Severe"='#ff7f00',
  #                            "Severe"='#e41a1c')) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05), add=0), 
                     labels=function(x){prettyNum(x, big.mark=',')},
                     limits=c(0, max(totals$value))) +
  theme(legend.title=element_blank()) + 
  labs(y="Number of People in Food Insecurity", title="", 
       x='Year', color='')

ggsave('Blog_Graph.png', width=7, height=4)
