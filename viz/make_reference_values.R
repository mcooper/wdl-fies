setwd('~/wdl-fies');library(ProjectTemplate);load.project()

rsq <- function(y, yhat){
  1 - (sum((y - yhat)^2))/sum((y - mean(yhat))^2)
}

rf_cv_mod_rsq <- round(rsq(moddat$fies.mod, moddat$fies.mod.pred.cv), 3)
rf_cv_mod_mae <- round(mean(abs(moddat$fies.mod - moddat$fies.mod.pred.cv)), 3)
rf_cv_sev_rsq <- round(rsq(moddat$fies.sev, moddat$fies.sev.pred.cv), 3)
rf_cv_sev_mae <- round(mean(abs(moddat$fies.sev - moddat$fies.sev.pred.cv)), 3)

rf_wthn_mod_rsq <- round(rsq(moddat$fies.mod, moddat$fies.mod.pred), 3)
rf_wthn_mod_mae <- round(mean(abs(moddat$fies.mod - moddat$fies.mod.pred)), 3)
rf_wthn_sev_rsq <- round(rsq(moddat$fies.sev, moddat$fies.sev.pred), 3)
rf_wthn_sev_mae <- round(mean(abs(moddat$fies.sev - moddat$fies.sev.pred)), 3)

ls_cv_mod_rsq <- round(rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred.cv), 3)
ls_cv_mod_mae <- round(mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred.cv)), 3)
ls_cv_sev_rsq <- round(rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred.cv), 3)
ls_cv_sev_mae <- round(mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred.cv)), 3)

ls_wthn_mod_rsq <- round(rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred), 3)
ls_wthn_mod_mae <- round(mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred)), 3)
ls_wthn_sev_rsq <- round(rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred), 3)
ls_wthn_sev_mae <- round(mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred)), 3)


#Global headcounts
hc <- preddat %>%
  group_by(YEAR) %>%
  summarize(sevtot=sum(population*fies.sev.pred),
            modtot=sum(population*fies.mod.pred))

#In millions
glob_2020_sev <- round(hc$sevtot[hc$YEAR == 2020]/1000000, -1)
#In billions
glob_2020_mod <- round(hc$modtot[hc$YEAR == 2020]/1000000000, 1)

pct_chng <- function(x1, x2){
  ((x2 - x1)/x1)*100
}

decprc_2010_2030_mod <- abs(round(pct_chng(hc$modtot[hc$YEAR == 2010],hc$modtot[hc$YEAR == 2030]), 1))
decprc_2010_2030_sev <- abs(round(pct_chng(hc$sevtot[hc$YEAR == 2010],hc$sevtot[hc$YEAR == 2030]), 1))

sev_tot_2019 <- round(hc$sevtot[hc$YEAR == 2019]/1000000)
sev_tot_2030 <- round(hc$sevtot[hc$YEAR == 2030]/1000000)
decrprc_2019_2030_sev <- abs(round(pct_chng(hc$sevtot[hc$YEAR == 2019],hc$sevtot[hc$YEAR == 2030]), 1))

val <- c(rf_cv_mod_rsq,
rf_cv_mod_mae, 
rf_cv_sev_rsq, 
rf_cv_sev_mae, 
rf_wthn_mod_rsq, 
rf_wthn_mod_mae, 
rf_wthn_sev_rsq, 
rf_wthn_sev_mae, 
ls_cv_mod_rsq, 
ls_cv_mod_mae, 
ls_cv_sev_rsq, 
ls_cv_sev_mae, 
ls_wthn_mod_rsq, 
ls_wthn_mod_mae, 
ls_wthn_sev_rsq, 
ls_wthn_sev_mae, 
glob_2020_sev, 
glob_2020_mod, 
decprc_2010_2030_mod, 
decprc_2010_2030_sev, 
sev_tot_2019, 
sev_tot_2030, 
decrprc_2019_2030_sev)

var <- c("rf_cv_mod_rsq",
"rf_cv_mod_mae", 
"rf_cv_sev_rsq", 
"rf_cv_sev_mae", 
"rf_wthn_mod_rsq", 
"rf_wthn_mod_mae", 
"rf_wthn_sev_rsq", 
"rf_wthn_sev_mae", 
"ls_cv_mod_rsq", 
"ls_cv_mod_mae", 
"ls_cv_sev_rsq", 
"ls_cv_sev_mae", 
"ls_wthn_mod_rsq", 
"ls_wthn_mod_mae", 
"ls_wthn_sev_rsq", 
"ls_wthn_sev_mae", 
"glob_2020_sev", 
"glob_2020_mod", 
"decprc_2010_2030_mod", 
"decprc_2010_2030_sev", 
"sev_tot_2019", 
"sev_tot_2030", 
"decrprc_2019_2030_sev")

df <- data.frame(var, val)

write.csv(df, 'docs/vars.csv', row.names=F)

