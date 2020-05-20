library(ProjectTemplate)
setwd('C://Users/matt/wdl-fies')
load.project()

subindicators <- 
  data.frame(
    matrix(
      c('WORRIED', 'During the last 12 MONTHS, was there a time when\nyou were worried you would not have enough food to eat\nbecause of a lack of money or other resources?', 
        'HEALTHY', 'Still thinking about the last 12 MONTHS, was there\na time when you were unable to eat healthy and nutritious\nfood because of a lack of money or other resources?', 
        'FEWFOOD', 'Was there a time when you ate only a few kinds of\nfoods because of a lack of money or other resources?', 
        'SKIPPED', 'Was there a time when you had to skip a meal because\nthere was not enough money or other resources to get food?', 
        'ATELESS', 'Still thinking about the last 12 MONTHS, was there a\ntime when you ate less than you thought you should\nbecause of a lack of money or other resources?', 
        'RUNOUT', 'Was there a time when your household ran out of food\nbecause of a lack of money or other resources?', 
        'HUNGRY', 'Was there a time when you were hungry but did not eat\nbecause there was not enough money or other\nresources for food?',
        'WHLDAY', 'During the last 12 MONTHS, was there a time when you\nwent without eating for a whole day because of\na lack of money or other resources?'
      ),
      byrow=T,
      ncol = 2
    )
  )

names(subindicators) <- c('code', 'question')

for (i in 1:nrow(subindicators)){
  res <- country_map(fies_raw, 'country', subindicators$code[i])
  
  res <- res + 
    labs(title = subindicators$question[i])

  ggsave(plot=res, filename=paste0('figures/subindicator - ', subindicators$code[i], '.png'), width = 12, height=7)  
}



