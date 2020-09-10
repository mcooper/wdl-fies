#######################################################
# Use Bayes SSVS with SSP covariates
# to estimate food insecurity, based on data for
# modeled for 2020, 2025, and 2030
#######################################################

# read in data
moddat <- merge(fies_subnat, 
                covars, 
                all.x=T, all.y=F) %>%
  na.omit %>%
  data.frame

preddat <- covars %>%
  data.frame

exc <- c('crops_prod', 'forest', 'builtup', 'livestock', #8
         'pasture', 'cropland')

# set up model
vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
                                            'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
                                            'urban', 'rural', 'fies.sev', 'fies.mod',
                                            'population', 'YEAR', 'rural_perc', 'region',
                                            names(moddat)[grepl('region', names(moddat))],
                                            exc)]

moddat <- moddat %>%
  mutate(fies.mod = logit(fies.mod),
         fies.sev = logit(fies.sev))


##--------------------------------------------------------------------------------------------------------------------------------------
## bayesian from different tutorials, combination of "BaysianOLS.R" and "SSVS.R"
## OLS for normal OLS and SSVS for model selection (shrinkage, like LASSO)

bayes.ssvs <- function(y, bigX, nburn, nsave, ssvs, fig) {
  
  # ssvs <- 1 #scale the distributions of the spike and slab prior, something between 1-10 maybe, 1 is standard (from course)
  # 
  # # bring data into corrrect format
  # data <- moddat %>%
  #   select(fies.mod, fies.sev, vars)
  # summary(data)
  # 
  # # model moderate food insecurity
  # y <- as.matrix(data %>% select(fies.mod))
  # bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
  # colnames(bigX) <- c("(Intercept)", vars)
  # nburn <- 10000         #number of burn ins
  # nsave <- 10000         #number of saved draws
  # 
  # fig <- T

  y <- y
  X <- bigX
  N <- nrow(X)
  
  # ------------------ #
  # --- Check data --- #
  # ------------------ #
  
  # let's look at OLS estimates
  # coefficient estimates
  # (X'X)^(-1) X'y
  beta.ols   <- solve(t(X) %*% X, tol = rcond(t(X) %*% X)*0.99999) %*% t(X) %*% y

  # variance estimate
  errors.ols <- y - X %*% beta.ols
  sig2.ols   <- t(errors.ols) %*% errors.ols / (N-ncol(X))

  # you can get the same via Rs linear regression function
  # note the -1 is necessary as we already included an intercept in X
  reg <- lm(y~X-1)
  summary(reg)
  
  
  # --------------------------- #
  # --- GIBBS SAMPLER SETUP --- #
  # --------------------------- #
  
  nsave <- nsave         #number of saved draws
  nburn <- nburn         #number of burn ins
  ntot  <- nsave + nburn #number of total iterations
  P     <- ncol(X)       #number of explanatories
  N     <- nrow(X)       #number of observations
  
  
  # ------------------- #
  # --- PRIOR SETUP --- #
  # ------------------- #
  
  # ssvs prior on coefficients
  sig2h <- 10*ssvs         #ssvs slab  (high) variance
  sig2l <- .01/ssvs        #ssvs spike (low)  variance
  
  b0    <- matrix(0, nrow=P)   #prior mean of beta
  B0    <- diag(sig2h,  P)     #prior var-cov of beta
  B0inv <- diag(1/sig2h, P)    #inverse of prior var-cov of beta
  
  # how does our normal - normal spike and slab prior look like?
  #par(mfrow=c(1,1),mar=c(1,1,1,1))
  if(fig == T) {png("figures/bayes_ssvs/spike_slab_prior_bayes_ssvs.png")}
  plot(density(rnorm(10000, 0, sqrt(sig2h))),ylim=c(0,2)) #slab
  lines(density(rnorm(10000, 0, sqrt(sig2l))), col="red")   #spike
  if(fig == T) {dev.off()}
  
  # inverse gamma prior on the variance
  c0 <- 2 #prior shape of sigma2
  C0 <- 1 #prior rate  of sigma2
  
  # how does this prior distribution look like?
  #png("figures/bayes_ssvs/variance_prior_bayes_ssvs_wt.png")
  plot(density(1/rgamma(20000, c0, C0)))
  #dev.off()
  
  
  # note that we implicitly set prior inclusion probability for all variables to 0.5
  # could also implement a version with other prior inclusion probabilities easily
  
  
  # ----------------------- #
  # --- STARTING VALUES --- #
  # ----------------------- #
  
  # set all inclusion indicators to 1
  delta.draw <- matrix(1, P, 1)
  
  # set variance to 1
  sig2.draw  <- 1
  
  # set coefficients to 0
  beta.draw  <- matrix(0, P, 1)
  
  # --------------- #
  # --- STORAGE --- #
  # --------------- #
  
  beta.store  <- matrix(NA, nsave, P)
  sig2.store  <- matrix(NA, nsave, 1)
  delta.store <- matrix(NA, nsave, P)
  
  # -------------------------------- #
  # --- GIBBS SAMPLING ALGORITHM --- #
  # -------------------------------- #
  
  # compute sufficient statistics
  XX <- t(X)%*%X
  XY <- t(X)%*%y
  
  # progess bar
  pb <- txtProgressBar(min = 0, max = ntot, style = 3)
  
  
  for (irep in 1:ntot){ # MCMC LOOP START
    
    # STEP 1: SAMPLE BETA GIVEN SIGMA & DATA
    
    # compute posterior quantities
    Bn_inv <- B0inv + XX / sig2.draw
    Bn     <- solve(Bn_inv)
    bn     <- Bn %*% (B0inv %*% b0 + XY / sig2.draw)
    
    # draw from a multivariate normal distribution
    # (there are several ways to do that, here I use a package for simplicity)
    beta.draw <- mnormt::rmnorm(1, bn, Bn)
    
    
    # STEP 2: SAMPLE SIGMA2 GIVEN BETA & DATA
    
    # compute errors
    e  <- y - X %*% beta.draw
    # sum of squared residuals (equivalent to sum((e)^2))
    ee <- t(e) %*% e
    
    # compute posterior quantities
    ck <- c0 +  N/2
    Ck <- C0 + ee/2
    
    # sample sigma2 from inverse gamma posterior
    sig2.draw <- 1/rgamma(1, ck, Ck)
    
    
    
    # STEP 3: STOCHASTIC SEARCH VARIABLE SELECTION
    
    # this will be WAY more numerically stable if you do everything in log space
    # however, for didactic purposes i stay away from the logs
    # it's easier to see what happens in that way
    
    # likelihood that coefficients come from spike component (=exclusion)
    ll.spike    <- dnorm(beta.draw, 0, sqrt(sig2l))
    
    # likelihood that coefficients come from slab component (=inclusion)
    ll.slab     <- dnorm(beta.draw, 0, sqrt(sig2h))
    
    # compute inclusion probability
    # to get probabilities, we have to normalize!
    # here, we could in principal add our prior and multiply it with the likelihood
    # implicitly, prior inclusion probability = 0.5 and it drops out of the equation!
    pip         <- ll.slab / (ll.spike + ll.slab)
    
    # now we sample our delta inclusion indicators by flipping a coin with
    # success probability being equal to the posterior inclusion probability
    delta.draw  <- ifelse(pip > runif(P), 1, 0)
    
    # finally, use the inclusion indicator delta to update your prior var-cov 
    # coefficients that are included get the large variance
    # coefficients that are excluded get the low variance
    scaling     <- delta.draw * sig2h + (1-delta.draw) * sig2l
    B0inv       <- diag(1 / scaling, P)
    
    
    # STEP 4: STORE POSTERIOR SAMPLES
    
    #only after burn-in period!
    if(irep > nburn){
      
      beta.store[irep-nburn,]  <- beta.draw
      sig2.store[irep-nburn,]  <- sig2.draw
      delta.store[irep-nburn,] <- delta.draw 
      
    }
    
    
    # STEP 4: PROGRESS
    # (in case you want to know how many iterations we already did)
    if(irep%%(ntot/100)==0){  
      #Sys.sleep(0.0001)
      # update progress bar
      setTxtProgressBar(pb, irep)}
    
  } # MCMC LOOP END
  
  
  # -------------------------- #
  # --- MCMC CONVERGENCE ----- #
  # -------------------------- #
  
  #coefficient 1, true value and OLS estimate
  plot.ts(beta.store[,1])
  abline(a=beta.ols[1],b=0, col="green")
  
  #coefficient 2, true value and OLS estimate
  plot.ts(beta.store[,ncol(beta.store)])
  abline(a=beta.ols[ncol(beta.store)],b=0, col="green")
  
  
  # -------------------------- #
  # --- PIP ANALYSIS --------- #
  # -------------------------- #
  
  # we can now look at the posterior inclusion probabilities for all coefficients
  # these are simply the average over all draws for delta
  pip.post  <- apply(delta.store,2, mean)
  beta.post <- apply(beta.store, 2, mean)
  
  cbind(round(beta.post,2), round(pip.post,2))
  #View(cbind(colnames(X), round(beta.post,2), round(pip.post,2))[order(-pip.post),])
  
  
  # -------------------------- #
  # --- POSTERIOR ANALYSIS --- #
  # -------------------------- #
  
  # usually, the reader is not interested in you plotting full posterior distributions
  # we can use a few statistics to summarize the posterior distribution of any given parameter:
  
  # ... plot all posterior distributions
  if(fig == T) {png("figures/bayes_ssvs/posterior_bayes_ssvs.png")}
  par(mfrow=c(ceiling(P/2),2),mar=c(2,2,2,2))
  for(i in 1:P) {
    plot(density(beta.store[,i]), main = colnames(X)[i], 
         xlim = c(median(beta.store[,i])-3*sd(beta.store[,i]),median(beta.store[,i])+3*sd(beta.store[,i])))
    abline(v=mean(beta.store[,i]), col="red")
    abline(v=median(beta.store[,i]), col="green")
    abline(v=quantile(beta.store[,i], prob = 0.05), col="blue", lty=2)
    abline(v=quantile(beta.store[,i], prob = 0.95), col="blue", lty=2)
  }
  if(fig == T) {dev.off()}
  
  if(fig == T) {png("figures/bayes_ssvs/covar_box_bayes_ssvs.png")}
  par(mfrow=c(ceiling(P/2),2),mar=c(2,2,2,2))
  for(i in 1:P) {
    boxplot(beta.store[,i], main = colnames(X)[i], horizontal = T)
  }
  if(fig == T) {dev.off()}
  
  
  # ... posterior means
  apply(beta.store, 2, mean) #how does that compare to ols estimates?
  
  # ... posterior medians
  apply(beta.store, 2, median)
  
  # ... posterior standard deviations
  apply(beta.store, 2, sd)
  
  # ... highest posterior density intervals / credible intervals (nice interpretation!)
  apply(beta.store, 2, quantile, prob = c(0.05,0.95))
  
  # summerize output
  df <- data.frame(
    term = colnames(X), 
    mean = apply(beta.store, 2, mean), #beta mean
    pip = apply(delta.store,2, mean), #posterior inclusion probability
    median = apply(beta.store, 2, median), #beta median
    sd = apply(beta.store, 2, sd), #standard deviation
    
    cred0.05 = apply(beta.store, 2, quantile, prob = 0.05), #posterior density intervals, 0.05 and 0.95
    cred0.95 = apply(beta.store, 2, quantile, prob = 0.95),
    cred0.25 = apply(beta.store, 2, quantile, prob = 0.25), #posterior density intervals, 0.25 and 0.75
    cred0.75 = apply(beta.store, 2, quantile, prob = 0.75)
  )
  
  return(df)
}

##--------------------------------------------------------------------------------------------------------------------------------------

ssvs <- 1 #scale the distributions of the spike and slab prior, something between 1-10 maybe, 1 is standard (from tutorial) 

# bring data into corrrect format
data <- moddat %>% 
  select(fies.mod, fies.sev, vars)
summary(data)

# model moderate food insecurity
y <- as.matrix(data %>% select(fies.mod))
bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
colnames(bigX) <- c("(Intercept)", vars)
nburn <- 10000         #number of burn ins
nsave <- 90000         #number of saved draws

fig <- T
mdf <- bayes.ssvs(y, bigX, nburn, nsave, ssvs, fig)

# model severe food insecurity
y <- as.matrix(data %>% select(fies.sev))
bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
colnames(bigX) <- c("(Intercept)", vars)
nburn <- 10000         #number of burn ins
nsave <- 90000         #number of saved draws

fig <- F
sdf <- bayes.ssvs(y, bigX, nburn, nsave, ssvs, fig)


# get model predictions
for(x in c("mean", "median", "cred0.05", "cred0.95", "cred0.25", "cred0.75")) {
  preddat[[paste0("fies.mod.pred_",x)]] <- mdf[[x]][mdf$term == '(Intercept)']
  moddat[[paste0("fies.mod.pred_",x)]] <- mdf[[x]][mdf$term == '(Intercept)']
  preddat[[paste0("fies.sev.pred_",x)]] <- sdf[[x]][sdf$term == '(Intercept)']
  moddat[[paste0("fies.sev.pred_",x)]] <- sdf[[x]][sdf$term == '(Intercept)']
  
  for (i in 2:nrow(mdf)){
    preddat[[paste0("fies.mod.pred_",x)]] <- preddat[[paste0("fies.mod.pred_",x)]] + preddat[ , mdf$term[i]]*mdf[[x]][i]
    moddat[[paste0("fies.mod.pred_",x)]] <- moddat[[paste0("fies.mod.pred_",x)]] + moddat[ , mdf$term[i]]*mdf[[x]][i]
    preddat[[paste0("fies.sev.pred_",x)]] <- preddat[[paste0("fies.sev.pred_",x)]] + preddat[ , sdf$term[i]]*sdf[[x]][i]
    moddat[[paste0("fies.sev.pred_",x)]] <- moddat[[paste0("fies.sev.pred_",x)]] + moddat[ , sdf$term[i]]*sdf[[x]][i]
  }
  
  preddat[[paste0("fies.mod.pred_",x)]] <- inv.logit(preddat[[paste0("fies.mod.pred_",x)]])
  moddat[[paste0("fies.mod.pred_",x)]] <- inv.logit(moddat[[paste0("fies.mod.pred_",x)]])
  preddat[[paste0("fies.sev.pred_",x)]] <- inv.logit(preddat[[paste0("fies.sev.pred_",x)]])
  moddat[[paste0("fies.sev.pred_",x)]] <- inv.logit(moddat[[paste0("fies.sev.pred_",x)]])
  
}

moddat <- moddat %>%
  mutate(fies.mod = inv.logit(fies.mod),
         fies.sev = inv.logit(fies.sev))


# take the mean as "true" point estimate
moddat <- moddat %>% rename(fies.mod.pred = "fies.mod.pred_mean",
                              fies.sev.pred = "fies.sev.pred_mean")

preddat <- preddat %>% rename(fies.mod.pred = "fies.mod.pred_mean",
                              fies.sev.pred = "fies.sev.pred_mean")


##################################
# Model specific outputs/plots
##################################

# assess residuals
mae <- mean(abs(moddat$fies.mod - moddat$fies.mod.pred))
r2 <- cor(moddat$fies.mod, moddat$fies.mod.pred)
ggplot(moddat) + 
  geom_point(aes(x=fies.mod, y=fies.mod.pred)) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (bayes_ssvs)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Mod+Sev Food Insecurity',
       y='Modeled Rate of Mod+Sev Food Insecurity')	
ggsave('figures/bayes_ssvs/Residuals.Mod_bayes_ssvs.png', width=5, height=5)

mae <- mean(abs(moddat$fies.sev - moddat$fies.sev.pred))
r2 <- cor(moddat$fies.sev, moddat$fies.sev.pred)
ggplot(moddat) +
  geom_point(aes(x=fies.sev, y=fies.sev.pred)) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 0.5) +
  labs(title='Model Performance (bayes_ssvs)',
       caption=paste0('Mean Absolute Error: ',  round(mae, 4),
                      '\nR-squared: ', round(r2, 4)),
       x='Observed Rates of Sev Food Insecurity',
       y='Modeled Rate of Sev Food Insecurity')
ggsave('figures/bayes_ssvs/Residuals.Sev_bayes_ssvs.png', width=5, height=5)



# plot scaled covariates
mean_effect <- mdf$mean[mdf$term =='(Intercept)']
for (v in vars){
  if (v %in% mdf$term){
    mdf$scaled[mdf$term==v] <- mdf$median[mdf$term==v]*sd(moddat[ , v])
  } else{
    mdf <- bind_rows(mdf, data.frame(term=v, median=0, scaled=0))
  }
  mean_effect <- mean_effect + mean(covars[[v]], na.rm=T)*mdf$mean[mdf$term == v]
}
mdf$ame <- inv.logit(mdf$scale + mean_effect) - inv.logit(mean_effect)
mdf$term <- factor(mdf$term, levels=mdf$term[order(mdf$scaled)], ordered=TRUE)

ggplot(mdf %>% filter(term != '(Intercept)')) + 
  geom_bar(aes(x=term, y=ame), stat='identity') + 
  coord_flip() + 
  labs(title='Average Change in Rate of Mod+Sev Food Insecurity\nWith increase of 1 SD in Var (bayes_ssvs)',
       x="", y="") + 
  theme_minimal()
ggsave('figures/bayes_ssvs/Coef.Mod_bayes_ssvs.png', width=6, height=5)


mean_effect <- sdf$mean[sdf$term =='(Intercept)']
for (v in vars){
  if (v %in% sdf$term){
    sdf$scaled[sdf$term==v] <- sdf$median[sdf$term==v]*sd(moddat[ , v])
  } else{
    sdf <- bind_rows(sdf, data.frame(term=v, median=0, scaled=0))
  }
  mean_effect <- mean_effect + mean(covars[[v]], na.rm=T)*sdf$mean[sdf$term == v]
}
sdf$ame <- inv.logit(sdf$scale + mean_effect) - inv.logit(mean_effect)
sdf$term <- factor(sdf$term, levels=sdf$term[order(sdf$scaled)], ordered=TRUE)

ggplot(sdf %>% filter(term != '(Intercept)')) +
  geom_bar(aes(x=term, y=ame), stat='identity') +
  coord_flip() +
  labs(title='Average Change in Sev Food Insecurity\nWith increase of 1 SD in Var (bayes_ssvs)',
       x="", y="") +
  theme_minimal()
ggsave('figures/bayes_SSVS/Coefs.Sev_bayes_ssvs.png', width=6, height=5)



# credible interval
cred <- preddat %>%
  filter(YEAR > 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.total_median=sum(fies.mod.pred_median * (population), na.rm=T),
            mod.total_cred0.25=sum(fies.mod.pred_cred0.25 * (population), na.rm=T),
            mod.total_cred0.75=sum(fies.mod.pred_cred0.75 * (population), na.rm=T)) 

ggplot(cred) +
  geom_line(aes(x=YEAR, y=mod.total_median), size = 1) +
  geom_line(aes(x=YEAR, y=mod.total_cred0.25), size = 0.8, linetype = "dashed") +
  geom_line(aes(x=YEAR, y=mod.total_cred0.75), size = 0.8, linetype = "dashed") +
  scale_x_continuous(expand=c(0,0), limits=c(2011, 2030.5)) +
  scale_y_continuous(expand=c(0,0), limits=c(1*10^9, 4*10^9), labels = scales::comma) +
  labs(x='Year', y="Number Food Insecure",
       title="Number With Moderate or Severe Food Insecurity\nwith 0.25/0.75 Credible Intervals (bayes_ssvs)") +
  theme_bw() +
  theme(legend.title=element_blank())
ggsave('figures/bayes_ssvs/Time.Mod.Cred_bayes_ssvs.png', width=8, height=5)



# upper and lower
uplow <- preddat %>%
  filter(YEAR > 2010) %>%
  group_by(YEAR) %>%
  summarize(mod.total_median=sum(fies.mod.pred_median * (population), na.rm=T),
            mod.total_cred0.25=sum(fies.mod.pred_cred0.25 * (population), na.rm=T),
            mod.total_cred0.75=sum(fies.mod.pred_cred0.75 * (population), na.rm=T))  %>%
  gather(var, value, -YEAR)

mapdat <- merge(gdl, 
                preddat %>% 
                  filter(YEAR %in% c(2020, 2025, 2030)) %>%
                  select(GDLCODE, YEAR, fies.mod.pred_cred0.25, fies.mod.pred_cred0.75), 
                all.x=T, all.y=F) %>%
  merge(uplow %>%
          filter(YEAR %in% c(2020, 2025, 2030)) %>%
          group_by(YEAR, var) %>%
          summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
          spread(var, value) %>%
          mutate(mod25.YEAR = paste0(YEAR, ':\n', mod.total_cred0.25, 
                                   '\nIn Moderate or Severe Food Insecurity'),
                 mod75.YEAR = paste0(YEAR, ':\n', mod.total_cred0.75, 
                                   '\nIn Moderate or Severe Food Insecurity')))

countries <- ne_countries(returnclass='sf')
ggplot(mapdat) + 
  geom_sf(aes(fill=fies.mod.pred_cred0.25), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2\nLower Bound at 0.25-Credible Interval (bayes_ssvs)',
       fill='') + 
  facet_grid(mod25.YEAR ~ .)
ggsave('figures/bayes_ssvs/Map.Mod.Cred0.25_bayes_ssvs.png', width=10, height=12)


ggplot(mapdat) + 
  geom_sf(aes(fill=fies.mod.pred_cred0.75), color=NA) + 
  scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
                                 "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
  geom_sf(data=countries, color='#000000', fill=NA) + 
  coord_sf(crs='+proj=robin') + 
  theme_void() + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) + 
  labs(title='Rate of Moderate or Severe Food Insecurity Under SSP2\nUpper Bound at 0.75-Credible Interval (bayes_ssvs)',
       fill='') + 
  facet_grid(mod75.YEAR ~ .)
ggsave('figures/bayes_ssvs/Map.Mod.Cred0.75_bayes_ssvs.png', width=10, height=12)


m <- "bayes_ssvs"

