# -------------------------------------------------- #
#                                                    #
# Macroeconometrics Science Track Summer Term 2020   #
# Metropolis - Hastings- Algorithm                   #
# Gregor Zens                                        #
# May 2020                                           #
#                                                    #
# ---------------------------------------------------#

# again, most of the things done here can be done in a more efficient or shorter way
# i introduce the MH step in many sub-steps here to make clear what is happening

# note that some refer to what we do here as "Metropolis-within-Gibbs"
# this means that we use the MH algorithm as one step in a Gibbs sampler
# it can be shown that this generates valid posterior samples


# --------------------- #
# --- SIMULATE DATA --- #
# --------------------- #

# this section simulates an ar(1) process

# let's set a seed such that the results are reproducible
set.seed(1234)

t         <- 200                             #how many observations?
true.rho  <- 0.9                             #true ar parameter
sig2.true <- 0.75                            #true error variance
y1        <- rnorm(1,0, sqrt(sig2.true))     #some starting value for dependent variable


# now simulate dependent variable y in a loop
y         <- matrix(NA, t, 1)
y[1,]     <- y1

for(tt in 2:t){ #note that loop runs from 2 to t
  
y[tt,]    <- true.rho * y[tt-1,] + rnorm(1, 0, sqrt(sig2.true))  
  
}

# look at the process we've just simulated

plot.ts(y)

# create our "explanatory variables", which is only the lagged dependent variable

X <- y[-nrow(y),,drop=F] #note that the drop=F prevents R from discarding dimension of 1

# cut first observation of y (no lag available for this period)

y <- y[-1, , drop=F]

# what have we just accomplished?

cbind(y,X)

# ---------------------------- #
# --- CHECK SIMULATED DATA --- # (important!)
# ---------------------------- #

# let's estimate an ar(1) process using R functions

arima(y, order = c(1,0,0))

# looking good


# --------------------------- #
# --- GIBBS SAMPLER SETUP --- #
# --------------------------- #

nsave <- 10000         #number of saved draws
nburn <- 10000         #number of burn ins
ntot  <- nsave + nburn #number of total iterations
P     <- ncol(X)       #number of explanatories
t     <- nrow(X)       #number of observations
eta2  <- 0.0165        #proposal variance for metropolis hastings step

# ------------------- #
# --- PRIOR SETUP --- #
# ------------------- #

# uniform prior U(a0,b0) on rho

a0 <- -1 #lower bound on uniform
b0 <-  1 #upper bound on uniform

# how does this prior look like?
# you can play a bit around to get a feeling for a beta distribution
plot(density(runif(20000, a0, b0))) 

# inverse gamma prior on the variance
c0 <- 2 #prior shape of sigma2
C0 <- 1 #prior rate  of sigma2

# how does this prior distribution look like?
plot(density(1/rgamma(20000, c0, C0)))


# ----------------------- #
# --- STARTING VALUES --- #
# ----------------------- #

# set variance to 1
sig2.draw <- 1

# set rho to 0
rho.draw  <- 0

# we will also use an acceptance "counter" to compute acceptance ratio
accept    <- 0

# ----------------------- #
# --- STORAGE ----------- #
# ----------------------- #

rho.store  <- matrix(NA, nsave, 1)
sig2.store <- matrix(NA, nsave, 1)

# ------------------------------ #
# --- GIBBS SAMPLING ALGORITHM - #
# ------------------------------ #


for (irep in 1:ntot){ # MCMC LOOP START

  # STEP 1: SAMPLE RHO USING A METROPOLIS HASTINGS STEP
  
    # create a proposal for rho
    rho.prop <- rnorm(1, rho.draw, sqrt(eta2))
    
    # now we will evaluate the current draw and the proposal in the posterior density
    # this implies evaluating a) the uniform(a0,b0) prior and b) the normal likelihood
    # note that everything we do happens in LOG SPACE, otherwise things quickly go towards 0
    
    # evaluate the uniform prior for current draw and proposal
    prior.current <- dunif(rho.draw, a0, b0, log = T)
    prior.prop    <- dunif(rho.prop, a0, b0, log = T)
  
    # evaluate the normal likelihood for current draw and proposal
    ll.current    <- dnorm(y, X * rho.draw, sqrt(sig2.draw), log = T)
    ll.prop       <- dnorm(y, X * rho.prop, sqrt(sig2.draw), log = T)
    
    # note that we now evaluated every single observation
    # to get overall likelihood, we would take the product of all likelihoods
    # equivalently, we take the sum of all log likelihoods here
    ll.current    <- sum(ll.current)
    ll.prop       <- sum(ll.prop)
    
    # now we can combine the prior and likelihood evaluations to 
    # get conditional posterior evaluation
    # note that prior * likelihood becomes log(prior) + log(likelihood)
    c.p.current   <- ll.current + prior.current
    c.p.prop      <- ll.prop    + prior.prop
    
    # finally, we can compute the likelihood ratio
    # in log space, this likelihood ratio becomes the difference of the log-likelihoods!
    ll.ratio      <- c.p.prop - c.p.current
  
    # now we do a Bayesian coinflip with a biased coin
    # generate a random number from a uniform distribution
    # and see whether our likelihood ratio is higher
    # (note: this has nothing to do with the uniform prior we put on rho)
    # note that this corresponds to the "rules" of the MH-algorithm
    # if likelihood ratio > 1 -> accept (uniform draw can be 1 at max)
    # if likelihood ratio < 1 -> accept with probability of likelihood ratio
    # again, we do everything in log space
    
    if(ll.ratio > log(runif(1))){ #if our proposal is likely "enough"
      
      #replace the current draw for rho with our proposal
      rho.draw <- rho.prop 
      
      #add 1 to our acceptance counter
      accept   <- accept + 1
      
    } else { #if our proposal is not likely "enough"
      
      #note that this part of the if-else-statement is completey unnecessary and redundant
      #however, i want you to see what happens in both cases explicitly
      
      #keep your current draw for rho as draw for rho
      rho.draw <- rho.draw
      
      # add nothing to our acceptance counter
      accept   <- accept + 0
    }
    
    
    
    # STEP 2: SAMPLE SIGMA2 GIVEN RHO & DATA
  
    # compute errors
    e  <- y - X * rho.draw
    # sum of squared residuals (equivalent to sum((e)^2))
    ee <- t(e) %*% e
  
    # compute posterior quantities
    ck <- c0 +  t/2
    Ck <- C0 + ee/2
  
    # sample sigma2 from inverse gamma posterior
    sig2.draw <- 1/rgamma(1, ck, Ck)
    
  # STEP 3: STORE POSTERIOR SAMPLES
    
    #only after burn-in period!
    
    if(irep > nburn){
      
    rho.store[irep-nburn,]  <- rho.draw
    sig2.store[irep-nburn,] <- sig2.draw
      
    }

    
  # STEP 4: PROGRESS
    if(irep%%50==0){print(irep)}
    
    
} # MCMC LOOP END



# -------------------------- #
# --- MCMC CONVERGENCE ----- #
# -------------------------- #

# let's look at the traceplots and density for rho

# rho and true value
plot.ts(rho.store)
abline(a=true.rho,b=0, col="green")

# rho and true value
plot(density(rho.store))
abline(v=true.rho, col = "green")


# -------------------------- #
# --- ACCEPTANCE RATIO ----- #
# -------------------------- #

# how many of our proposals have been accepted?
# use our accept counter and the number of total iterations 

accept / ntot

# this is an extremely crucial number when tuning your MH steps
# if the acceptance ratio is very high, this means most of your proposals are accepted
# if the acceptance ratio is very low, this means most of your proposals are rejected

# try it out, play around with the proposal variance!

# ---------------------------------------- #
# --- TUNING THE MH STEP - THE PROBLEM --- #
# ---------------------------------------- #

# tuning the MH step refers to changing your proposal variance

# a very small proposal variance will result in many accepted proposals
# at the same time, the posterior distribution will be explored extremely slowly
# (set the proposal variance extremely small and see what happens)

# a very large proposal variance will result in very little accepted proposals
# at the same time, the posterior distribution will be explored in a pretty fast manner, 
# IF you manage to generate a proposal that gets accepted
# (set the proposal variance extremely large and see what happens)

# intuitively, finding a good proposal variance means to find a good tradeoff between making to many small accepted steps and making to many large proposals that get rejected


# ----------------------------------------- #
# --- TUNING THE MH STEP - THE SOLUTION --- #
# ----------------------------------------- #

# how can we find an ''ideal'' proposal variance that combines many accepts + fast exploration?

# method 1 - "the hard worker": very often, MH steps are tuned manually. that means, play around with the proposal variance until it suits your problem at hand.

# method 2 - "the automatizer": there are methods out there to automatically tune your proposal variance on the fly. one example are ''adaptive metropolis hastings'' algorithms that are actually rather easy to implement. a good starting point is this paper: Haario et al. (2001): 'An adaptive Metropolis algorithm' https://projecteuclid.org/euclid.bj/1080222083

# another crude method is to work with if-else statements during burn-in: if acceptance rate is too low during last X draws, set proposal variance a bit lower. if acceptance rate is too high during last X draws, set proposal variance a bit higher.

# but still, what is a ''good'' tradeoff? what is a ''good'' acceptance rate?
# as a rule of thumb, aim for 20% - 50% acceptance ratio. 
# if you want a more precise definition, some very important people in the Bayes-community have shown that asymptotically, you should aim for an acceptance ratio of 0.234 (https://projecteuclid.org/DPubS?service=UI&version=1.0&verb=Display&handle=euclid.aoap/1034625254)
