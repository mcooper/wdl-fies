# -------------------------------------------------- #
#                                                    #
# Macroeconometrics Science Track Summer Term 2020   #
# Stochastic Search Variable Selection               #
# Gregor Zens                                        #
# April 2020                                         #
#                                                    #
# ---------------------------------------------------#


# --------------------- #
# --- SIMULATE DATA --- #
# --------------------- #

# let's set a seed such that the results are reproducible
set.seed(999)

P         <- 5                                   #how many informative explanatories
Q         <- 90                                  #how many noise explanatories
N         <- 100                                 #how many observations?
intercept <- matrix(1, nrow = N, ncol = 1)       #intercept
x1        <- matrix(rnorm(N*(P+Q-1)), N, P+Q-1)  #normally distributed regressors
true.beta <- c(rnorm(P,0,5), rep(0, Q))          #true coefficients
sig2.true <- 0.75                                #true error variance
errors    <- rnorm(N, 0, sqrt(sig2.true))        #error terms
X         <- cbind(intercept, x1)                #explanatory matrix

# now simulate dependent variable y using beta, X and errors

y         <- X %*% true.beta + errors

# ---------------------------- #
# --- CHECK SIMULATED DATA --- # (important!)
# ---------------------------- #

# let's look at OLS estimates 
# coefficient estimates
# (X'X)^(-1) X'y
beta.ols   <- solve(t(X) %*% X) %*% t(X) %*% y

# variance estimate
errors.ols <- y - X %*% beta.ols
sig2.ols   <- t(errors.ols) %*% errors.ols / (N-ncol(X))


# --------------------#
#     CRIME EXAMPLE   #
# --------------------#

load("./models/bayes_tutorial/crime.rda")
reg <- lm(y~X-1)
ses <- sqrt(diag(vcov(reg))) # ALTERNATIVE: USE SCALED OLS STANDARD ERRORS AS SCALES (* 10, * 0.01)

# --------------------------- #
# --- GIBBS SAMPLER SETUP --- #
# --------------------------- #

nsave <- 5000         #number of saved draws
nburn <- 5000         #number of burn ins
ntot  <- nsave + nburn #number of total iterations
P     <- ncol(X)       #number of explanatories
N     <- nrow(X)       #number of observations


# ------------------- #
# --- PRIOR SETUP --- #
# ------------------- #

# ssvs prior on coefficients

sig2h <- 10         #ssvs slab  (high) variance
sig2l <- .01        #ssvs spike (low)  variance

b0    <- matrix(0, nrow=P)   #prior mean of beta
B0    <- diag(sig2h,  P)     #prior var-cov of beta
B0inv <- diag(1/sig2h, P)    #inverse of prior var-cov of beta

# how does our normal - normal spike and slab prior look like?
plot(density(rnorm(10000, 0, sqrt(sig2h))),ylim=c(0,0.8)) #slab
lines(density(rnorm(10000, 0, sqrt(sig2l))), col="red")   #spike

# inverse gamma prior on the variance

c0 <- 2 #prior shape of sigma2
C0 <- 1 #prior rate  of sigma2

# how does this prior distribution look like?
plot(density(1/rgamma(20000, c0, C0)))


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

# ----------------------- #
# --- STORAGE ----------- #
# ----------------------- #

beta.store  <- matrix(NA, nsave, P)
sig2.store  <- matrix(NA, nsave, 1)
delta.store <- matrix(NA, nsave, P)

# ------------------------------ #
# --- GIBBS SAMPLING ALGORITHM - #
# ------------------------------ #

# compute sufficient statistics
XX <- t(X)%*%X
XY <- t(X)%*%y


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
    if(irep%%50==0){print(irep)}
    
} # MCMC LOOP END



# -------------------------- #
# --- MCMC CONVERGENCE ----- #
# -------------------------- #

#coefficient 1, true value and OLS estimate
plot.ts(beta.store[,1])
abline(a=true.beta[1], b=0, col="red")
abline(a=beta.ols[1],b=0, col="green")

#coefficient 2, true value and OLS estimate
plot.ts(beta.store[,15])
abline(a=true.beta[15], b=0, col="red")
abline(a=beta.ols[15],b=0, col="green")


# -------------------------- #
# --- PIP ANALYSIS --------- #
# -------------------------- #

# we can now look at the posterior inclusion probabilities for all coefficients
# these are simply the average over all draws for delta

pip.post  <- apply(delta.store,2, mean)
beta.post <- apply(beta.store, 2, mean)

cbind(round(beta.post,2), round(pip.post,2))

# CRIME EXAMPLE
View(cbind(colnames(X), round(beta.post,2), round(pip.post,2))[order(-pip.post),])


# hence, ssvs is able to detect unnecessary variables and shrinks them to 0
# by applying a very tight normal prior centered at 0 to all excluded variables

# this effect will become more and more visible when N becomes small and P becomes large

# let's compare OLS estimates and SSVS estimates for N = 100, 5 relevant and 90 irrelevant variables

plot(pip.post)  # posterior inclusion probabilities for all variables

plot(true.beta) # true coefficients
points(beta.ols,  col = "red")  #red dots for OLS estimates
points(beta.post, col = "blue") #blue dots for SSVS estimates

plot(beta.ols, true.beta)
abline(a=0,b=1)

plot(beta.post, true.beta)
abline(a=0,b=1)

# that's what people mean when they talk about shrinkage & regularization
# it is clearly visible how the SSVS reduces noise during estimation 
# therefore, better estimates for the other coefficients can be obtained as well


# if there's still time: house price example!