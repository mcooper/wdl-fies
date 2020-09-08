# -------------------------------------------------- #
#                                                    #
# Macroeconometrics Science Track Summer Term 2020   #
# Bayesian Vector Autoregressions                    #
# Maximilian Boeck                                   #
# May 2020                                           #
#                                                    #
# ---------------------------------------------------#

###----------------------------------- Load packages -------------------------------------------------

if(suppressWarnings(!require(bvarsv))){
  install.packages("bvarsv")
}

if(suppressWarnings(!require(MCMCpack))){
  install.packages("MCMCpack")
}

if(suppressWarnings(!require(magic))){
  install.packages("magic")
}

require(bvarsv)
type <- "real" # either "sim" or "real" 
###----------------------------------- Load/Simulate data ----------------------------------------------------
if(type == "sim"){
  set.seed(571) #set seed to obtain the same results
  # DGP of VAR(1) process
  N <- 3 # number of endogenous variables
  persist <- 0.9 # define true coefficient of first own lag
  TT <- 100 # define total number of observations
  p <- 1
  
  #store object 
  Yraw <- matrix(0, TT, N)
  colnames(Yraw) <- paste0("Var", 1:N)
  Yraw[1,] <- rnorm(N) #define intial observation
  
  A.mat <- matrix(0,N,N) # coefficient matrix 
  A.mat <- matrix(rnorm(N^2, 0, 0.1), N, N)
  diag(A.mat) <- persist #first own lag
  
  eig.re <- max(Re(eigen(A.mat)$values))
  eig.im <- max(Im(eigen(A.mat)$values))
  
  Sig.low <- matrix(0,N,N) #variance-covariance matrix
  diag(Sig.low) <- 1 # true variance
  Sig.low[lower.tri(Sig.low, diag = FALSE)] <- rnorm((N*(N-1)/2), 0, 0.1) #true lower Cholesky factor
  Sig <- Sig.low%*%t(Sig.low)
  
  constant <- rnorm(N,0,1)
  
  for (tt in 2:TT){
    Yraw[tt,] <- constant + A.mat%*%Yraw[tt-1,]+Sig.low%*%rnorm(N,0,1)
  }
}else{
  if(type == "real"){
    #Real macro data of US, using the bvarsv package
    data("usmacro")
    Traw <- nrow(usmacro)
    Yraw <- usmacro
  }
}

plot.ts(Yraw)
#------------------------------------------------------------------------------------
# useful function
mlag <- function(X,lag){
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0,Traw,p*N)
  for (ii in 1:p){
    Xlag[(p+1):Traw,(N*(ii-1)+1):(N*ii)]=X[(p+1-ii):(Traw-ii),(1:N)]
  }
  return(Xlag)  
}
#######################################################################################
### Independent normal-Wishart prior for the VAR
######################################################################################
# needed libraries
library(MCMCpack) # has the inverse wishart riwish()
library(magic)
# specifications
p <- 2               # number of lags

# Create data matrices
Y <- as.matrix(Yraw)
X <- cbind(mlag(Y,p),1)

# look at size of data
dim(Y)
dim(X)

#first p rows are zero | conditioning on the first p observations
Y <- Y[(p+1):nrow(Y),]
X <- X[(p+1):nrow(X),]
y <- as.vector(Y)

# outside loop calculations
XX <- crossprod(X)

# get useful dimensions
M    <- ncol(Y)           # number of endogenous variables in the VAR
bigT <- nrow(Y)           # sample size, do not use T!
K    <- ncol(X)           # number of parameters per equation
k    <- K*M               # total number of coefficients
#------------------------------------------------------------------------------------
# Initial Values, OLS preliminaries
#------------------------------------------------------------------------------------
A_OLS <- solve(crossprod(X))%*%crossprod(X,Y)
S_OLS <- crossprod(Y-X%*%A_OLS)/(bigT-K)

# let's have a look at OLS estimates
yfit <- X%*%A_OLS
# amazing in-sample fit, but bad out-of-sample fit
library(zoo)
time <- as.yearqtr(time(usmacro))
par(mfrow=c(3,1))
plot.ts(Y[,1], xlab="", ylab="", main="Inflation", xaxt="n", lwd=2, cex.main=1.5, cex.lab=2)
lines(yfit[,1],col="red", lwd=2)
axis(1, at=seq(1,195,by=20), labels=time[seq(1,195,by=20)], las=2)
abline(v=seq(1,195,by=20), col="lightgrey", lty=3)
plot.ts(Y[,2], xlab="", ylab="", main="Unemployment", xaxt="n", lwd=2, cex.main=1.5, cex.lab=2)
lines(yfit[,2],col="red", lwd=2)
axis(1, at=seq(1,195,by=20), labels=time[seq(1,195,by=20)], las=2)
abline(v=seq(1,195,by=20), col="lightgrey", lty=3)
plot.ts(Y[,3], xlab="", ylab="", main="T-bills", xaxt="n", lwd=2, cex.main=1.5, cex.lab=2)
lines(yfit[,3],col="red", lwd=2)
axis(1, at=seq(1,195,by=20), labels=time[seq(1,195,by=20)], las=2)
abline(v=seq(1,195,by=20), col="lightgrey", lty=3)

# explained variation
diag(crossprod(yfit))/diag(crossprod(Y))

# initialize draws with OLS estimators
A_draw <- A_OLS
S_draw <- S_OLS

# get AR variances
sigs <- numeric(length=M)
for(mm in 1:M){
  yuse <- Y[,mm,drop=FALSE]
  xuse <- cbind(X[,seq(mm,M*p,by=M),drop=FALSE],1)
  b    <- solve(crossprod(xuse))%*%crossprod(xuse,yuse)
  sigs[mm] <- crossprod(yuse-xuse%*%b)/(bigT-p-1)
}
#------------------------------------------------------------------------------------
# PRIORS
#------------------------------------------------------------------------------------
A_prior <- matrix(0,K,M)
diag(A_prior) <- 1
a_prior <- as.vector(A_prior)

# Minnesota prior
# own lags:       (lambda1/k)^2   # k == lag
# cross lags:     (sig_i^2/sig_j^2)(lambda2/k)^2
# deterministics: lambda3*sig_i^2
lambda1 <- 0.1; lambda2 <- 0.2; lambda3 <- 100
V_prior <- array(0,c(K,K,M))
for(mm in 1:M){ # over all equations
  for(pp in 1:p){ # over all lags
    for(mmm in 1:M){ # over all coefficients
      # own lag
      if(mm==mmm){
        V_prior[(pp-1)*M+mmm,(pp-1)*M+mmm,mm] <- (lambda1/pp)^2
      }else{
        V_prior[(pp-1)*M+mmm,(pp-1)*M+mmm,mm] <- (sigs[mm]/sigs[mmm])*(lambda2/pp)^2
      }
    }
  }
  V_prior[K,K,mm] <- lambda3*sigs[mm]
}
V_prior    <- lapply(seq(dim(V_prior)[3]), function(x) V_prior[,,x]) # array to list
V_prior    <- Reduce(magic::adiag,V_prior)
V_priorinv <- diag(1/diag(V_prior))

# hyperparamter for inverse Wishart
s0 <- M + 2
S0 <- (s0 - M - 1)*diag(sigs)

# outside loop calculations
s_post    <- bigT + s0
#------------------------------------------------------------------------------------
# MCMC setup
#------------------------------------------------------------------------------------
nsave <- 1000               # number of saved draws
nburn <- 1000               # number of burned draws
ntot  <- nsave + nburn      # number of total draws
nhor  <- 13                 # horizon for IRFs
fhorz <- 8                  # forecasting horizon

# Container for MCMC draws
A_store <- array(NA, c(nsave, K, M))
S_store <- array(NA, c(nsave, M, M))

eig_store <- numeric(length=nsave)
cou_store <- numeric(length=nsave)

# Predictions -- dimensions: number of draws x number of variables x forecasting horizon
yf_store <- array(NA, c(nsave,M,fhorz))

# IRFs -- dimensions: Number of draws x Number of responses x Number of structural shocks x horizon
IRFchol_store <- array(NA,c(nsave,M,M,nhor))
IRFsign_store <- array(NA,c(nsave,M,M,nhor))

set.seed(1)
for(irep in 1:ntot){
  # Step 1: Draw S_draw | Y, A_draw from IW
  # s_overbar = T + s_underbar (s0)
  # S_overbar = (Y-XA)'(Y-XA) + S_underbar (S0)
  # SIGMA | Y ~ iW(s_overbar,S_overbar)
  
  S_post    <- crossprod(Y-X%*%A_draw)
  S_drawinv <- matrix(rWishart(1,s_post,solve(S_post)),M,M) # note that we can also draw from the Wishart
  S_draw    <- solve(S_drawinv)                            # and inverting leads to the inverse-Wishart
  # or using the MCMCpack
  S_draw    <- MCMCpack::riwish(s_post, S_post)
  
  # Step 2: Draw A_draw | Y, S_draw from MVN
  # V_overbar = (SIGMAinv otimes X'X + Vinv_underbar)^{-1}
  # a_overbar = V_overbar (Vinv_underbar a_underbar + (SIGMAinv otimes X')y)
  V_post    <- solve(kronecker(S_drawinv, XX) + V_priorinv)
  A_post    <- V_post %*% (kronecker(S_drawinv, t(X))%*%y + V_priorinv%*%a_prior)
  
  A_draw     <- matrix(A_post + t(chol(V_post))%*%rnorm(k),K,M)
  
  # Step 3: Storage/Predictions/IRFs
  if(irep > nburn){
    # Step 3a: Save draws
    A_store[irep-nburn,,] <- A_draw
    S_store[irep-nburn,,] <- S_draw
    
    # Step 3b: Companion Matrix
    Cm <- matrix(0,M*p,M*p)
    Cm[1:M,] <- t(A_draw[1:(M*p),])
    diag(Cm[(M+1):(M*p),1:(M*(p-1))]) <- 1
    Jm <- matrix(0,M*p,M)
    diag(Jm) <- 1
    
    # Step 3c: Do predictions and calculate fhorz-step ahead prediction density
    Mean00  <- c(Y[bigT,],X[bigT,1:(M*(p-1))])
    Sigma00 <- matrix(0,M*p,M*p)
    for(ihorz in 1:fhorz){
      # first and second moments
      Mean00  <- Cm%*%Mean00
      Sigma00 <- Cm%*%Sigma00%*%t(Cm)+Jm%*%S_draw%*%t(Jm)
      
      yf    <- Mean00[1:M] + t(chol(Sigma00[1:M,1:M]))%*%rnorm(M)
      yf_store[irep-nburn,,ihorz] <- yf
    }
    
    # Step 3d: Check stability
    eig_store[irep-nburn] <- max(Re(eigen(Cm)$values))
    
    # Step 3e: Impulse response functions
    cond.overall <- TRUE
    counter      <- 0
    MaxTries     <- 1000
    
    #Identification via Cholesky
    shock.chol <- t(chol(S_draw))
    #Normalise shocks to 1 units
    shock.chol <- shock.chol%*%diag(1/diag(shock.chol))
    
    # draw rotation matrices Q till you find a fitting one (while-loop)
    while(cond.overall && counter < MaxTries){
      counter <- counter + 1
      # Define a rotation matrix with positive values on the main diagonal
      Rtilda <- matrix(rnorm(M^2,0,1),M,M)
      qr.object <- qr(Rtilda)
      Q <- qr.Q(qr.object)
      Q <- Q%*%diag((diag(Q)>0)-(diag(Q)<0)) 
      
      #shock is a full matrix
      shock.sign <- shock.chol%*%Q
      
      # Monetary Policy shock
      cond.MP <- (shock.sign[1,3]<0)*(shock.sign[2,3]>0)*(shock.sign[3,3]>0)
      # Oil price shock // aggregate supply shock
      cond.AS <- (shock.sign[1,1]>0)*(shock.sign[2,1]>0)*(shock.sign[3,1]<0)
      # Demand shock
      cond.AD <- (shock.sign[1,2]<0)*(shock.sign[2,2]>0)*(shock.sign[3,2]<0)
      #Shocks have to be mutually exclusive (orthogonal)
      
      cond.overall <- (cond.MP*cond.AS*cond.AD)==0
    }
    cou_store[irep-nburn] <- counter
    
    # Temporary objects for state space representation
    irf.mat.chol<- irf.mat.sign  <- array(NA,c(M,M,nhor))
    
    # Impulse --> shock at t = 0:
    irf.mat.chol[,,1] <- shock.chol 
    irf.mat.sign[,,1] <- shock.sign
    
    #start at t = 1, as t = 0 is the impulse shock
    Cmi <- Cm
    for(ihorz in 2:nhor){
      irf.mat.chol[,,ihorz] <- t(Jm)%*%Cmi%*%Jm%*%shock.chol
      irf.mat.sign[,,ihorz] <- t(Jm)%*%Cmi%*%Jm%*%shock.sign
      Cmi <- Cmi%*%Cm
    }
    IRFchol_store[irep-nburn,,,] <- irf.mat.chol
    IRFsign_store[irep-nburn,,,] <- irf.mat.sign
  }
  if(irep%%50==0) print(paste0("Round: ", irep,"/",ntot))
}

# CHECK CONVERGENCE
library(coda)
crit_val <- 1.96
Z_scores <- c()

par(mfrow=c(K,3),mar=c(1,1,1,1))
for(jj in 1:M){
  for(ii in 1:K){
    plot.ts(A_store[,ii,jj])
    Z_scores[(jj-1)*K+ii] <- geweke.diag(A_store[,ii,jj])$z
  }
}

par(mfrow=c(K,3),mar=c(1,1,1,1))
for(jj in 1:M){
  for(ii in 1:K){
    acf(A_store[,ii,jj])
  }
}

par(mfrow=c(3,3),mar=c(1,1,1,1))
for(jj in 1:M){
  for(ii in 1:M){
    plot.ts(S_store[,ii,jj])
    Z_scores[k+(jj-1)*M+ii] <- geweke.diag(S_store[,ii,jj])$z
  }
}

par(mfrow=c(M,M),mar=c(1,1,1,1))
for(jj in 1:M){
  for(ii in 1:M){
    acf(S_store[,ii,jj])
  }
}

idx<-which(abs(Z_scores)>crit_val)
paste(length(idx), " out of ",k+M^2, " variables' z-values exceed the 1.96 threshold", " (", round(length(idx)/(k+M^2)*100,2),"%)",sep="")

#Quantiles over the first dimension (number of saved draws)
IRFchol_low    <- apply(IRFchol_store, c(2,3,4), quantile, 0.16,na.rm=TRUE)
IRFchol_high   <- apply(IRFchol_store, c(2,3,4), quantile, 0.84,na.rm=TRUE)
IRFchol_median <- apply(IRFchol_store, c(2,3,4), median, na.rm=TRUE)

IRFsign_low    <- apply(IRFsign_store, c(2,3,4), quantile, 0.16,na.rm=TRUE)
IRFsign_high   <- apply(IRFsign_store, c(2,3,4), quantile, 0.84,na.rm=TRUE)
IRFsign_median <- apply(IRFsign_store, c(2,3,4), median, na.rm=TRUE)

#Start plotting the IRFs w.r.t. different shocks
par(mfrow=c(3,3))
for(ii in 1:M){
  for(jj in 1:M){
    min1 <- min(IRFchol_low[ii,jj,])
    max1 <- max(IRFchol_high[ii,jj,])
    plot.ts(IRFchol_median[ii,jj,], ylab =colnames(Y)[[ii]], main=colnames(Y)[[jj]], ylim = c(min1,max1),xaxt="n",lwd=2) 
    lines(IRFchol_low[ii,jj,], lty = 2, lwd=2)
    lines(IRFchol_high[ii,jj,], lty = 2, lwd=2)
    abline(h=0,col="red",lwd=2)
    abline(v=seq(1,nhor,by=2), col="lightgrey", lty=2)
    axis(1, at=seq(1,nhor,by=2), labels=seq(0,nhor-1,by=2))
  }
}

par(mfrow=c(3,3))
for(ii in 1:M){
  for(jj in 1:M){
    min1 <- min(IRFsign_low[ii,jj,])
    max1 <- max(IRFsign_high[ii,jj,])
    plot.ts(IRFsign_median[ii,jj,], ylab =colnames(Y)[[ii]], main=colnames(Y)[[jj]], ylim = c(min1,max1), xaxt="n",lwd=2) 
    lines(IRFsign_low[ii,jj,], lty = 2, lwd=2)
    lines(IRFsign_high[ii,jj,], lty = 2, lwd=2)
    abline(h=0,col="red",lwd=2)
    abline(v=seq(1,nhor,by=2), col="lightgrey", lty=2)
    axis(1, at=seq(1,nhor,by=2), labels=seq(0,nhor-1,by=2))
  }
}

## check signs
par(mfrow=c(1,1))
hist(cou_store)

### plotting predictions
yf_low    <- apply(yf_store, c(2,3), quantile, 0.16, na.rm=TRUE)
yf_median <- apply(yf_store, c(2,3), quantile, 0.50, na.rm=TRUE)
yf_high   <- apply(yf_store, c(2,3), quantile, 0.84, na.rm=TRUE)

yf_low    <- cbind(t(Yraw[(bigT-5):bigT,]),yf_low)
yf_median <- cbind(t(Yraw[(bigT-5):bigT,]),yf_median)
yf_high   <- cbind(t(Yraw[(bigT-5):bigT,]),yf_high)

xax <- c(as.character(time[(bigT-5):bigT]),paste0("t+",seq(1,fhorz)))

par(mfrow=c(3,1),mar=c(5,3,2,3))
for(ii in 1:M){
  min1 <- min(yf_low[ii,])
  max1 <- max(yf_high[ii,])
  plot.ts(yf_median[ii,], ylim=c(min1,max1), main=colnames(Y)[[ii]], ylab="", xlab="", xaxt="n",lwd=2)
  lines(yf_low[ii,], lty=2,lwd=2)
  lines(yf_high[ii,], lty=2,lwd=2)
  lines(yf_median[ii,], lty=1,lwd=2)
  axis(1, at=seq(1,14), labels=xax, las=2)
  abline(v=seq(1,14), col="lightgrey", lty=2)
}

