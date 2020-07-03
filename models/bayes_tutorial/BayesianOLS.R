# -------------------------------------------------- #
#                                                    #
# Macroeconometrics Science Track Summer Term 2020   #
# Bayesian OLS using Gibbs Sampling                  #
# Gregor Zens                                        #
# April 2020                                         #
#                                                    #
# ---------------------------------------------------#

# Note: This code is certainly not the most efficient way to estimate Bayesian OLS. Didactics > Efficiency in this file.

# --------------------- #
# --- PACKAGES & AUX -- #
# --------------------- #

# the following line installs a package for sampling from a multivariate normal distribution I'll be using:
#install.packages("mnormt")

# --------------------- #
# --- SIMULATE DATA --- #
# --------------------- #

# install.packages("mnormt")
library(mnormt)

# let's set a seed such that the results are reproducible
set.seed(1234)

N         <- 1000                            #how many observations?
intercept <- matrix(1, nrow = N, ncol = 1)   #intercept
x1        <- rnorm(N)                        #normally distributed regressor
true.beta <- c(2, -1)                        #true coefficients
sig2.true <- 0.75                            #true error variance
errors    <- rnorm(N, 0, sqrt(sig2.true))    #error terms
X         <- cbind(intercept, x1)            #explanatory matrix

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

# you can get the same via Rs linear regression function
# note the -1 is necessary as we already included an intercept in X
  
reg <- lm(y~X-1)
summary(reg)

# alright, our simulated data is looking good
# now, let's start to set up a Gibbs sampling algorithm


# --------------------------- #
# --- GIBBS SAMPLER SETUP --- #
# --------------------------- #

# first, we have to throw in some general information that the sampler will use

nsave <- 10000         #number of saved draws
nburn <- 10000         #number of burn ins
ntot  <- nsave + nburn #number of total iterations
P     <- ncol(X)       #number of explanatories
N     <- nrow(X)       #number of observations

# ------------------- #
# --- PRIOR SETUP --- #
# ------------------- #

# second, we have to specify our prior distributions
# today, we will use conjugate priors
# next session will be about using arbitrary prior distributions

# normal priors on the coefficients

b0    <- matrix(0, nrow=P)  #prior mean of beta
B0    <- diag(100,  P)      #prior var-cov of beta
B0inv <- solve(B0)          #inverse of prior var-cov of beta

# how does this prior distribution look like?
plot(density(rnorm(20000, b0[1], B0[1,1])))

# inverse gamma prior on the variance

c0 <- 2 #prior shape of sigma2
C0 <- 1 #prior rate  of sigma2

# how does this prior distribution look like?
plot(density(1/rgamma(20000, c0, C0)))


# ----------------------- #
# --- STARTING VALUES --- #
# ----------------------- #

# in a next step, we have to provide some starting values for the Gibbs sampler
# we will stay agnostic and just use some arbitrary numbers
# you could also try using the OLS estimates as starting values if you want ''better'' starting values
# mathematically, it should not matter which values you pick

# set variance to 1
sig2.draw <- 1

# set coefficients to 0
beta.draw <- matrix(0, P, 1)

# ----------------------- #
# --- STORAGE ----------- #
# ----------------------- #

# finally, we have to create some containers where we can store the draws from the posterior distributions
# in each iteration, we get one draw for beta and one for sigma2
# we save many of them to be able to look at the simulated posterior distributions afterwards

beta.store <- matrix(NA, nsave, P)
sig2.store <- matrix(NA, nsave, 1)

# ------------------------------ #
# --- GIBBS SAMPLING ALGORITHM - #
# ------------------------------ #

# alright, the preliminary setup is done!
# now we will start to look into the Gibbs sampling algorithm
# we use a loop to iteratively draw from p(beta|sigma2,y,X) and p(sigma2|beta,y,X)

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
    
  # STEP 3: STORE POSTERIOR SAMPLES
    
    #only after burn-in period!
    
    if(irep > nburn){
      
    beta.store[irep-nburn,] <- beta.draw
    sig2.store[irep-nburn,] <- sig2.draw
      
    }

    
  # STEP 4: PROGRESS
    # (in case you want to know how many iterations we already did)
    if(irep%%50==0){print(irep)}
    
    # depending on how fancy your algorithm is supposed to be, there are progress bars available in R:
    # https://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
    
} # MCMC LOOP END



# -------------------------- #
# --- MCMC CONVERGENCE ----- #
# -------------------------- #

# did we converge? why is that important?
# theoretically, Gibbs sampler is guaranteed to converge
# however, if the posterior draws are heavily autocorrelated, a small posterior sample might give wrong conclusions

# hence, question of convergence is important. many possibilities to answer.
# eyeballing is important here!

# one possibility: look at traceplots
# you want caterpillars!

#coefficient 1, true value and OLS estimate
plot.ts(beta.store[,1])
abline(a=2, b=0, col="red")
abline(a=beta.ols[1],b=0, col="green")

#coefficient 2, true value and OLS estimate
plot.ts(beta.store[,2])
abline(a=-1, b=0, col="red")
abline(a=beta.ols[2],b=0, col="green")

# second possibility: look at posterior densities
# you want no distortions or weird/unexpected behavior

#error variance, true value and OLS estimate
plot(density(sig2.store))
abline(v=0.75, col="red")
abline(v=sig2.ols, col="green")

# there exist some 'formal' convergence criteria as well. The following paper compares 13 of them in case you
# are interested: http://people.ee.duke.edu/~lcarin/cowles96markov.pdf
# "We then compare their performance in two simple models and conclude that all the methods can fail to
# detect the sorts of convergence failure they were designed to identify.
# -> convergence is more of an art than a science
# however, one usually gets a good feeling for these things rather quickly


# anyway, convergence is looking good, let's talk about how we can summarize our estimation results


# -------------------------- #
# --- POSTERIOR ANALYSIS --- #
# -------------------------- #

# usually, the reader is not interested in you plotting full posterior distributions

# we can use a few statistics to summarize the posterior distribution of any given parameter:

# ... posterior means

apply(beta.store, 2, mean) #how does that compare to ols estimates?

# ... posterior medians

apply(beta.store, 2, median)

# ... posterior standard deviations

apply(beta.store, 2, sd)

# ... highest posterior density intervals / credible intervals (nice interpretation!)

apply(beta.store, 2, quantile, prob = c(0.05,0.95))



# -------------------------- #
# --- PRIOR INFLUENCE    --- #
# -------------------------- #

# when is a prior influential for posterior?
# usually, when we have 1) an informative likelihood (many observations, little noise) and 
# 2) a rather uninformative prior, the prior won't matter
# at the end of the day, you only multiply two densities

# however, in case your data is not informative, prior might matter
# alternatively, if your prior is very informative, it will matter
# try to simulate only a few observations and/or play around with a more informative prior to see what happens!

