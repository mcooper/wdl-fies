setwd('~/wdl-fies');library(ProjectTemplate);load.project()

rsq <- function(y, yhat){
  1 - (sum((y - yhat)^2))/sum((y - mean(yhat))^2)
}

rf_cv_mod_rsq <- rsq(moddat$fies.mod, moddat$fies.mod.pred.cv)
rf_cv_mod_mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred.cv))
rf_cv_sev_rsq <- rsq(moddat$fies.sev, moddat$fies.sev.pred.cv)
rf_cv_sev_mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred.cv))

rf_wthn_mod_rsq <- rsq(moddat$fies.mod, moddat$fies.mod.pred)
rf_wthn_mod_mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
rf_wthn_sev_rsq <- rsq(moddat$fies.sev, moddat$fies.sev.pred)
rf_wthn_sev_mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))

ls_cv_mod_rsq <- rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred.cv)
ls_cv_mod_mae <- mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred.cv))
ls_cv_sev_rsq <- rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred.cv)
ls_cv_sev_mae <- mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred.cv))

ls_wthn_mod_rsq <- rsq(moddat.lasso$fies.mod, moddat.lasso$fies.mod.pred)
ls_wthn_mod_mae <- mean(abs(moddat.lasso$fies.mod - moddat.lasso$fies.mod.pred))
ls_wthn_sev_rsq <- rsq(moddat.lasso$fies.sev, moddat.lasso$fies.sev.pred)
ls_wthn_sev_mae <- mean(abs(moddat.lasso$fies.sev - moddat.lasso$fies.sev.pred))


glob_2020_sev
glob_2020_mod

incprc_2010_2030_mod
incprc_2010_2030_sev

sev_tot_2019
sev_tot_2030
sev_decrprc_2019_2030




















