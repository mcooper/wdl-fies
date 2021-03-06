# #######################################################
# # Use BMA with SSP covariates
# # to estimate food insecurity, based on data for
# # modeled for 2020, 2025, and 2030
# #######################################################
# 
# #Just to fix Siberia
# covars$wci_index[covars$GDLCODE=='RUSr108'] <- 1
# 
# covars$gdp_percap <- log(covars$gdp_percap)
# 
# for (i in unique(covars$region)){
#   if (i == 'HIC'){
#     next
#   }
#   covars[ , paste0('region', i)] <- as.numeric(covars$region == i)
# }
# 
# 
# # Read in data
# moddat <- merge(fies_subnat, 
#                 covars, 
#                 all.x=T, all.y=F) %>%
#   na.omit %>%
#   data.frame
# 
# preddat <- covars %>%
#   data.frame
# 
# exc <- c()
# #exc <- c('mal_falciparum', 'mal_vivax', 'wasting', 'crop_prod')
# exc <- c('crops_prod', 'forest', 'builtup', 'livestock',
#          'pasture', 'crops_prod', 'cropland',
#          'mal_vivax', 'stunting')
# 
# # Set up Model
# vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur',
#                                             'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',
#                                             'Urban', 'Rural', 'fies.sev', 'fies.mod',
#                                             'population', 'YEAR', 'rural_perc', 'region',
#                                             names(moddat)[grepl('region', names(moddat))],
#                                             exc)]
# 
# # vars <- names(moddat)[!names(moddat) %in% c('ISO3', 'GDLCODE', 'fies.mod.rur', 
# #                                             'fies.sev.rur', 'fies.mod.urb', 'fies.sev.urb',                                             
# #                                             'Urban', 'Rural', 'fies.mod', 'fies.sev', 
# #                                             'population', 'YEAR', 'rural_perc',
# #                                             'crops_prod', 'forest', 'builtup', 'livestock',
# #                                             'pasture', 'crops_prod', 'cropland', 
# #                                             'mal_vivax', 'stunting')]
# 
# 
# 
# ##--------------------------------------------------------------------------------------------------------------------------------------
# ## bayesian from different tutorials, combination of "BaysianOLS.R" and "SSVS.R"
# ## OLS for normal OLS and SSVS for model selection (shrinkage, like LASSO)
# 
# fun.bayesmagic <- function(y, bigX, nburn, nsave, ssvs) {
#   
#   # data <- moddat %>% 
#   #   select(fies.mod, fies.sev, vars) %>%
#   #   mutate(fies.mod = logit(fies.mod), fies.sev = logit(fies.sev))
#   # summary(data)
#   # 
#   # # model moderate food insecurity
#   # y <- as.matrix(data %>% select(fies.mod))
#   # bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
#   # colnames(bigX) <- c("(Intercept)", vars)
#   # nburn <- 10000         #number of burn ins
#   # nsave <- 10000         #number of saved draws
#   
#   y <- y
#   X <- bigX
#   N <- nrow(X)
#   
#   # ------------------ #
#   # --- Check data --- #
#   # ------------------ #
#   
#   # let's look at OLS estimates 
#   # coefficient estimates
#   # (X'X)^(-1) X'y
#   beta.ols   <- solve(t(X) %*% X) %*% t(X) %*% y
#   
#   # variance estimate
#   errors.ols <- y - X %*% beta.ols
#   sig2.ols   <- t(errors.ols) %*% errors.ols / (N-ncol(X))
#   
#   # you can get the same via Rs linear regression function
#   # note the -1 is necessary as we already included an intercept in X
#   reg <- lm(y~X-1)
#   summary(reg)
#   
#   
#   # --------------------------- #
#   # --- GIBBS SAMPLER SETUP --- #
#   # --------------------------- #
#   
#   nsave <- nsave         #number of saved draws
#   nburn <- nburn         #number of burn ins
#   ntot  <- nsave + nburn #number of total iterations
#   P     <- ncol(X)       #number of explanatories
#   N     <- nrow(X)       #number of observations
#   
#   
#   # ------------------- #
#   # --- PRIOR SETUP --- #
#   # ------------------- #
#   
#   # ssvs prior on coefficients
#   
#   sig2h <- 10*ssvs         #ssvs slab  (high) variance
#   sig2l <- .01/ssvs        #ssvs spike (low)  variance
#   
#   b0    <- matrix(0, nrow=P)   #prior mean of beta
#   B0    <- diag(sig2h,  P)     #prior var-cov of beta
#   B0inv <- diag(1/sig2h, P)    #inverse of prior var-cov of beta
#   
#   # how does our normal - normal spike and slab prior look like?
#   par(mfrow=c(1,1),mar=c(1,1,1,1))
#   plot(density(rnorm(10000, 0, sqrt(sig2h))),ylim=c(0,2)) #slab
#   lines(density(rnorm(10000, 0, sqrt(sig2l))), col="red")   #spike
#   
#   # inverse gamma prior on the variance
#   
#   c0 <- 2 #prior shape of sigma2
#   C0 <- 1 #prior rate  of sigma2
#   
#   # how does this prior distribution look like?
#   plot(density(1/rgamma(20000, c0, C0)))
#   
#   
#   # note that we implicitly set prior inclusion probability for all variables to 0.5
#   # could also implement a version with other prior inclusion probabilities easily
#   
#   
#   # ----------------------- #
#   # --- STARTING VALUES --- #
#   # ----------------------- #
#   
#   # set all inclusion indicators to 1
#   delta.draw <- matrix(1, P, 1)
#   
#   # set variance to 1
#   sig2.draw  <- 1
#   
#   # set coefficients to 0
#   beta.draw  <- matrix(0, P, 1)
#   
#   # --------------- #
#   # --- STORAGE --- #
#   # --------------- #
#   
#   beta.store  <- matrix(NA, nsave, P)
#   sig2.store  <- matrix(NA, nsave, 1)
#   delta.store <- matrix(NA, nsave, P)
#   
#   # -------------------------------- #
#   # --- GIBBS SAMPLING ALGORITHM --- #
#   # -------------------------------- #
#   
#   # compute sufficient statistics
#   XX <- t(X)%*%X
#   XY <- t(X)%*%y
#   
#   # progess bar
#   pb <- txtProgressBar(min = 0, max = ntot, style = 3)
#   
#   
#   for (irep in 1:ntot){ # MCMC LOOP START
#     
#     # STEP 1: SAMPLE BETA GIVEN SIGMA & DATA
#     
#     # compute posterior quantities
#     Bn_inv <- B0inv + XX / sig2.draw
#     Bn     <- solve(Bn_inv)
#     bn     <- Bn %*% (B0inv %*% b0 + XY / sig2.draw)
#     
#     # draw from a multivariate normal distribution
#     # (there are several ways to do that, here I use a package for simplicity)
#     beta.draw <- mnormt::rmnorm(1, bn, Bn)
#     
#     
#     # STEP 2: SAMPLE SIGMA2 GIVEN BETA & DATA
#     
#     # compute errors
#     e  <- y - X %*% beta.draw
#     # sum of squared residuals (equivalent to sum((e)^2))
#     ee <- t(e) %*% e
#     
#     # compute posterior quantities
#     ck <- c0 +  N/2
#     Ck <- C0 + ee/2
#     
#     # sample sigma2 from inverse gamma posterior
#     sig2.draw <- 1/rgamma(1, ck, Ck)
#     
#     
#     
#     # STEP 3: STOCHASTIC SEARCH VARIABLE SELECTION
#     
#     # this will be WAY more numerically stable if you do everything in log space
#     # however, for didactic purposes i stay away from the logs
#     # it's easier to see what happens in that way
#     
#     # likelihood that coefficients come from spike component (=exclusion)
#     
#     ll.spike    <- dnorm(beta.draw, 0, sqrt(sig2l))
#     
#     # likelihood that coefficients come from slab component (=inclusion)
#     
#     ll.slab     <- dnorm(beta.draw, 0, sqrt(sig2h))
#     
#     # compute inclusion probability
#     # to get probabilities, we have to normalize!
#     # here, we could in principal add our prior and multiply it with the likelihood
#     # implicitly, prior inclusion probability = 0.5 and it drops out of the equation!
#     
#     pip         <- ll.slab / (ll.spike + ll.slab)
#     
#     # now we sample our delta inclusion indicators by flipping a coin with
#     # success probability being equal to the posterior inclusion probability
#     
#     delta.draw  <- ifelse(pip > runif(P), 1, 0)
#     
#     # finally, use the inclusion indicator delta to update your prior var-cov 
#     # coefficients that are included get the large variance
#     # coefficients that are excluded get the low variance
#     
#     scaling     <- delta.draw * sig2h + (1-delta.draw) * sig2l
#     B0inv       <- diag(1 / scaling, P)
#     
#     
#     # STEP 4: STORE POSTERIOR SAMPLES
#     
#     #only after burn-in period!
#     
#     if(irep > nburn){
#       
#       beta.store[irep-nburn,]  <- beta.draw
#       sig2.store[irep-nburn,]  <- sig2.draw
#       delta.store[irep-nburn,] <- delta.draw 
#       
#     }
#     
#     
#     # STEP 4: PROGRESS
#     # (in case you want to know how many iterations we already did)
#     if(irep%%(ntot/100)==0){  
#       #Sys.sleep(0.0001)
#       # update progress bar
#       setTxtProgressBar(pb, irep)}
#     
#   } # MCMC LOOP END
#   
#   
#   # -------------------------- #
#   # --- MCMC CONVERGENCE ----- #
#   # -------------------------- #
#   
#   #coefficient 1, true value and OLS estimate
#   plot.ts(beta.store[,1])
#   # abline(a=true.beta[1], b=0, col="red")
#   abline(a=beta.ols[1],b=0, col="green")
#   
#   #coefficient 2, true value and OLS estimate
#   plot.ts(beta.store[,ncol(beta.store)])
#   # abline(a=true.beta[ncol(beta.store)], b=0, col="red")
#   abline(a=beta.ols[ncol(beta.store)],b=0, col="green")
#   
#   #looking good
#   
#   
#   # -------------------------- #
#   # --- PIP ANALYSIS --------- #
#   # -------------------------- #
#   
#   # we can now look at the posterior inclusion probabilities for all coefficients
#   # these are simply the average over all draws for delta
#   
#   pip.post  <- apply(delta.store,2, mean)
#   beta.post <- apply(beta.store, 2, mean)
#   
#   cbind(round(beta.post,2), round(pip.post,2))
#   #View(cbind(colnames(X), round(beta.post,2), round(pip.post,2))[order(-pip.post),])
#   
#   
#   # -------------------------- #
#   # --- POSTERIOR ANALYSIS --- #
#   # -------------------------- #
#   
#   # usually, the reader is not interested in you plotting full posterior distributions
#   # we can use a few statistics to summarize the posterior distribution of any given parameter:
#   
#   # ... plot all posterior distributions
#   
#   par(mfrow=c(P%/%2,2),mar=c(1,1,1,1))
#   for(i in 1:P) {
#     #i <- 1
#     plot(density(beta.store[,i]), main = colnames(X)[i], 
#          xlim = c(median(beta.store[,i])-3*sd(beta.store[,i]),median(beta.store[,i])+3*sd(beta.store[,i])))
#     abline(v=mean(beta.store[,i]), col="red")
#     abline(v=median(beta.store[,i]), col="green")
#     abline(v=quantile(beta.store[,i], prob = 0.05), col="blue", lty=2)
#     abline(v=quantile(beta.store[,i], prob = 0.95), col="blue", lty=2)
#   }
#   
#   # ... posterior means
#   
#   apply(beta.store, 2, mean) #how does that compare to ols estimates?
#   
#   # ... posterior medians
#   
#   apply(beta.store, 2, median)
#   
#   # ... posterior standard deviations
#   
#   apply(beta.store, 2, sd)
#   
#   # ... highest posterior density intervals / credible intervals (nice interpretation!)
#   
#   apply(beta.store, 2, quantile, prob = c(0.05,0.95))
#   
#   # summerize output
#   
#   df <- data.frame(
#     term = colnames(X), 
#     mean = apply(beta.store, 2, mean), #beta mean
#     pip = apply(delta.store,2, mean), #posterior inclusion probability
#     median = apply(beta.store, 2, median), #beta median
#     sd = apply(beta.store, 2, sd), #standard deviation
#     
#     conf0.05 = apply(beta.store, 2, quantile, prob = 0.05), #posterior density intervals, 0.05 and 0.95
#     conf0.95 = apply(beta.store, 2, quantile, prob = 0.95),
#     conf0.01 = apply(beta.store, 2, quantile, prob = 0.01), #posterior density intervals, 0.01 and 0.99
#     conf0.99 = apply(beta.store, 2, quantile, prob = 0.99)
#     
#     # conf0.05 = apply(beta.store, 2, FUN = function(x) {mean(x)-(qt(.05,length(x)-1)*sd(x)/sqrt(length(x)))}), #assume that they come from a normal?
#     # conf0.95 = apply(beta.store, 2, FUN = function(x) {mean(x)-(qt(.95,length(x)-1)*sd(x)/sqrt(length(x)))}),
#     # conf0.1 = apply(beta.store, 2, FUN = function(x) {mean(x)-(qt(.1,length(x)-1)*sd(x)/sqrt(length(x)))}),
#     # conf0.9 = apply(beta.store, 2, FUN = function(x) {mean(x)-(qt(.9,length(x)-1)*sd(x)/sqrt(length(x)))})
#   )
#   
#   # for(i in 1:P) {
#   #   if(mean(beta.store[,i]) < 0.001) {
#   #     df$conf0.05[i] <- 0
#   #     df$conf0.95[i] <- 0
#   #     df$conf0.01[i] <- 0
#   #     df$conf0.99[i] <- 0
#   #   }
#   # }
#   
#   # df <- df[order(-df$pip),]
#   
#   return(df)
# }
# 
# ##--------------------------------------------------------------------------------------------------------------------------------------
# 
# ssvs <- 1 #scale the distributions of the spike and slab prior, something between 1-10 maybe
# 
# # bring data into corrrect format
# data <- moddat %>% 
#   select(fies.mod, fies.sev, vars) %>%
#   mutate(fies.mod = logit(fies.mod), fies.sev = logit(fies.sev))
# summary(data)
# 
# # model moderate food insecurity
# y <- as.matrix(data %>% select(fies.mod))
# bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
# colnames(bigX) <- c("(Intercept)", vars)
# nburn <- 10000         #number of burn ins
# nsave <- 40000         #number of saved draws
# 
# mdf <- fun.bayesmagic(y, bigX, nburn, nsave, ssvs)
# 
# # model severe food insecurity
# y <- as.matrix(data %>% select(fies.sev))
# bigX <- cbind(matrix(1, nrow = nrow(data), ncol = 1), as.matrix(data %>% select(vars)))
# colnames(bigX) <- c("(Intercept)", vars)
# nburn <- 10000         #number of burn ins
# nsave <- 40000         #number of saved draws
# 
# sdf <- fun.bayesmagic(y, bigX, nburn, nsave, ssvs)
# 
# 
# # Get model predictions
# for(x in c("mean", "conf0.05", "conf0.95")) {
#   preddat[[paste0("fies.mod.pred_",x)]] <- mdf[[x]][mdf$term == '(Intercept)']
#   moddat[[paste0("fies.mod.pred_",x)]] <- mdf[[x]][mdf$term == '(Intercept)']
#   preddat[[paste0("fies.sev.pred_",x)]] <- sdf[[x]][sdf$term == '(Intercept)']
#   moddat[[paste0("fies.sev.pred_",x)]] <- sdf[[x]][sdf$term == '(Intercept)']
#   
#   for (i in 2:nrow(mdf)){
#     preddat[[paste0("fies.mod.pred_",x)]] <- preddat[[paste0("fies.mod.pred_",x)]] + preddat[ , mdf$term[i]]*mdf[[x]][i]
#     moddat[[paste0("fies.mod.pred_",x)]] <- moddat[[paste0("fies.mod.pred_",x)]] + moddat[ , mdf$term[i]]*mdf[[x]][i]
#     preddat[[paste0("fies.sev.pred_",x)]] <- preddat[[paste0("fies.sev.pred_",x)]] + preddat[ , sdf$term[i]]*sdf[[x]][i]
#     moddat[[paste0("fies.sev.pred_",x)]] <- moddat[[paste0("fies.sev.pred_",x)]] + moddat[ , sdf$term[i]]*sdf[[x]][i]
#   }
#   
#   preddat[[paste0("fies.mod.pred_",x)]] <- inv.logit(preddat[[paste0("fies.mod.pred_",x)]])
#   moddat[[paste0("fies.mod.pred_",x)]] <- inv.logit(moddat[[paste0("fies.mod.pred_",x)]])
#   preddat[[paste0("fies.sev.pred_",x)]] <- inv.logit(preddat[[paste0("fies.sev.pred_",x)]])
#   moddat[[paste0("fies.sev.pred_",x)]] <- inv.logit(moddat[[paste0("fies.sev.pred_",x)]])
#   
# }
# 
# 
# ############################
# # Visualize Results
# ############################
# 
# #####################
# # Time Series
# ###############
# 
# #Get totals by year
# totals <- preddat %>%
#   filter(YEAR > 2010) %>%
#   #group_by(YEAR) %>%
#   group_by(YEAR) %>%
#   summarize(mod.total_mean=sum(fies.mod.pred_mean * (population), na.rm=T),
#             mod.total_conf0.05=sum(fies.mod.pred_conf0.05 * (population), na.rm=T),
#             mod.total_conf0.95=sum(fies.mod.pred_conf0.95 * (population), na.rm=T),
#             
#             sev.total_mean=sum(fies.sev.pred_mean * (population), na.rm=T),
#             sev.total_conf0.05=sum(fies.sev.pred_conf0.05 * (population), na.rm=T),
#             sev.total_conf0.95=sum(fies.sev.pred_conf0.95 * (population), na.rm=T)
#             ) 
# 
# ggplot(totals) +
#   geom_line(aes(x=YEAR, y=mod.total_mean), size = 1) +
#   geom_line(aes(x=YEAR, y=mod.total_conf0.05), size = 0.8, linetype = "dashed") +
#   geom_line(aes(x=YEAR, y=mod.total_conf0.95), size = 0.8, linetype = "dashed") +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0), limits=c(1*10^9, 3*10^9)) +
#   labs(x='Year', y="Number Food Insecure",
#        title="Number With Moderate or Severe Food Insecurity with 0.05/0.95 Confidence Intervals") +
#   theme_bw()
# 
# #Get totals by year
# totals <- preddat %>%
#   filter(YEAR > 2010) %>%
#   #group_by(YEAR) %>%
#   group_by(YEAR, region) %>%
#   summarize(mod.total_mean=sum(fies.mod.pred_mean * (population), na.rm=T),
#             mod.total_conf0.05=sum(fies.mod.pred_conf0.05 * (population), na.rm=T),
#             mod.total_conf0.95=sum(fies.mod.pred_conf0.95 * (population), na.rm=T),
#             
#             sev.total_mean=sum(fies.sev.pred_mean * (population), na.rm=T),
#             sev.total_conf0.05=sum(fies.sev.pred_conf0.05 * (population), na.rm=T),
#             sev.total_conf0.95=sum(fies.sev.pred_conf0.95 * (population), na.rm=T)
#   )  %>%
#   gather(var, value, -YEAR, -region) #uncommant if coef int
# 
# 
# # ggplot(totals %>% filter(var=='mod.total')) +
# #   geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
# #   scale_x_continuous(expand=c(0,0)) +
# #   scale_y_continuous(expand=c(0,0)) +
# #   labs(x='Year', y="Number Food Insecure",
# #        title="Number With Moderate or Severe Food Insecurity, By Continent, Stacked") +
# #   theme_bw()
# # # ggsave('figures/Time.Mod.Stack.png', width=7, height=5)
# # 
# # ggplot(totals %>% filter(var=='mod.total')) +
# #   geom_line(aes(x=YEAR, y=value, color=region), size=1) +
# #   scale_x_continuous(expand=c(0,0)) +
# #   scale_y_continuous(expand=c(0,0)) +
# #   labs(x='Year', y="Number Food Insecure",
# #        title="Number With Moderate or Severe Food Insecurity, By Continent") +
# #   theme_bw()
# # # ggsave('figures/Time.Mod.Lines.png', width=7, height=5)
# # 
# # ggplot(totals %>% filter(var=='sev.total')) +
# #   geom_area(aes(x=YEAR, y=value, fill=region), position='stack') +
# #   scale_x_continuous(expand=c(0,0)) +
# #   scale_y_continuous(expand=c(0,0)) +
# #   labs(x='Year', y="Number Food Insecure",
# #        title="Number With Severe Food Insecurity, By Continent, Stacked") +
# #   theme_bw()
# # # ggsave('figures/Time.Sev.Stack.png', width=7, height=5)
# # 
# # ggplot(totals %>% filter(var=='sev.total')) +
# #   geom_line(aes(x=YEAR, y=value, color=region), size=1) +
# #   scale_x_continuous(expand=c(0,0)) +
# #   scale_y_continuous(expand=c(0,0)) +
# #   labs(x='Year', y="Number Food Insecure",
# #        title="Number With Severe Food Insecurity, By Continent") +
# #   theme_bw()
# # # ggsave('figures/Time.Sev.Lines.png', width=7, height=5)
# 
# 
# ############################
# # Make Map
# ##############################
# mapdat <- merge(gdl %>%
#                   select(-region), 
#                 preddat %>% 
#                   filter(YEAR %in% c(2020, 2025, 2030)) %>%
#                   select(GDLCODE, YEAR, fies.mod.pred_mean, fies.sev.pred_mean), 
#                 all.x=T, all.y=F) %>%
#   merge(totals %>%
#           filter(YEAR %in% c(2020, 2025, 2030)) %>%
#           group_by(YEAR, var) %>%
#           summarize(value=prettyNum(sum(value), scientific=F, big.mark=',')) %>%
#           spread(var, value) %>%
#           mutate(mod.YEAR = paste0(YEAR, ':\n', mod.total_mean, 
#                                    '\nIn Moderate or Severe Food Insecurity'),
#                  sev.YEAR = paste0(YEAR, ':\n', sev.total_mean, 
#                                    '\nIn Severe Food Insecurity')))
# 
# countries <- ne_countries(returnclass='sf')
# ggplot(mapdat) + 
#   geom_sf(aes(fill=fies.mod.pred_mean), color=NA) + 
#   scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
#                                  "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
#   geom_sf(data=countries, color='#000000', fill=NA) + 
#   coord_sf(crs='+proj=robin') + 
#   theme_void() + 
#   theme(legend.position = 'bottom',
#         plot.title = element_text(hjust = 0.5)) + 
#   labs(title='Rate of Moderate to Severe Food Insecurity Under SSP2',
#        fill='') + 
#   facet_wrap(mod.YEAR ~ .)
# # ggsave('figures/SSP2_LASSO_Moderate.png', width=20, height=8)
# 
# # ggplot(mapdat) + 
# #   geom_sf(aes(fill=fies.sev.pred), color=NA) + 
# #   scale_fill_gradientn(colours=c("#5e51a2", "#2f89be", "#66c3a6", "#add8a4", "#e4ea9a", "#fbf8c0", 
# #                                  "#fce08a", "#faae61", "#f36c44", "#a01c44")) + 
# #   geom_sf(data=countries, color='#000000', fill=NA) + 
# #   coord_sf(crs='+proj=robin') + 
# #   theme_void() + 
# #   theme(legend.position = 'bottom',
# #         plot.title = element_text(hjust = 0.5)) + 
# #   labs(title='Rate of Severe Food Insecurity Under SSP2',
# #        fill='') + 
# #   facet_wrap(sev.YEAR ~ .)
# # # ggsave('figures/SSP2_LASSO_Severe.png', width=20, height=8)
# 
# 
# 
# 
# rnormgamma <- function(n, mu, lambda, alpha, beta) {
#   bigT <- rgamma(n, alpha, beta)
#   x <- rnorm(n, mu, sqrt(1/(lambda*bigT)))
#   return(x)}
# 
# 
# 
# b <- rnormgamma(10000000, 0, 1, 0.5, 1)
# b <- rnorm(10000000, 0, 1)
# c <- rgamma(10000000, 0.5, 1)
# plot(density(b))
# 
# 
# 
# 
