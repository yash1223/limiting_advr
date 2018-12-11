
WriteModelOld <- function(
  output.dir = getwd(), # to save models and their output in different dirs
  modelname = "model",
  addarma  = FALSE
){
  modelpath <- file.path(output.dir, paste0(modelname, #ifelse(BDAprior, "BDAprior", ""), 
                              ".txt"))
  
  # start a model text file with "model{" in it
  cat(" 
    model {", 
  sep="", append=FALSE, file = modelpath, fill=TRUE)
  
  # add any additional model info XXXX using 
  #cat("XXX", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # data model
  cat("
    # this set up allows for doing cross-validation as well, indices getitrain.j are in the training set
   for (j in 1:ntrain){
    y.i[getitrain.j[j]] ~ dnorm(logmu.ct[getc.i[getitrain.j[j]], gett.i[getitrain.j[j]]], tauy.i[getitrain.j[j]])
   }
   for (i in 1:n){
# if you don't want to do validation runs, you can just use y.i here
#    y.i[i] ~ dnorm(mu.ct[getc.i[i], gett.i[i]], tauy.i[i])
    tauy.i[i] <- 1/(2*se.i[i]^2 + nonsamplsd^2)
    # for checks
    yrep.i[i] ~ dnorm(logmu.ct[getc.i[i], gett.i[i]], tauy.i[i])
    loglike.i[i] <- logdensity.norm(y.i[i], logmu.ct[getc.i[i], gett.i[i]], tauy.i[i])
   }
   nonsamplsd ~ dunif(0,0.1)
    ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 
    # hierarchical model
    for (c in 1:C){
      for (t in 1:nyears){
# for ratio, mu.ct is already on the log scale
    mu.ct[c,t] <- exp(logmu.ct[c, t])
      logmu.ct[c, t] <- logpi.ct[c, t] + delta.ct[c,t]
#logpi.ct[c, t] <- log(pi.ct[c, t])
#pi.ct[c, t] <- exp(logpimin1.ct[c, t])+1
      logpi.ct[c, t] <- alpha.c[c] + beta.c[c]*totaldemand.ct[c,t]
      }
      alpha.c[c] <- alphabeta.c2[c,1]
      beta.c[c] <- alphabeta.c2[c,2]
#      beta.c[c] <- beta1#alphabeta.r2[getr.c[c],2] #-exp(alphabeta.c2[c,2]) # constrain beta to be negative
      alphabeta.c2[c,1:2] ~ dmnorm(alphabeta.r2[getr.c[c], 1:2], InvSigma)
    } # end c loop
change ~ dbern(0.5)
timechange ~ dunif(0,26)
#gamma ~ dunif(-0.1,0) #ori scale
gamma ~ dunif(-0.5,0)


    Sigma[1,1] <- pow(sigma.alpha,2)
    Sigma[2,2] <- pow(sigma.beta,2)
    Sigma[1,2] <- rho.alphabeta*sigma.beta*sigma.alpha
    Sigma[2,1] <- Sigma[1,2]
    InvSigma[1:2, 1:2] <- inverse(Sigma[, ])
    # priors for cov matrix
    rho.alphabeta ~ dunif(-1,0) # upper bound based on run w/o ar
    sigma.alpha ~ dunif(0,3)
    sigma.beta ~ dunif(0,5)
#    sigma.beta ~ dunif(0.1,3) # lower bound based on run w/o ar
    for (r in 1:R){
      alphabeta.r2[r,1] ~ dnorm(beta0, tau.alphar)
      alphabeta.r2[r,2] ~ dnorm(beta1, tau.betar)
    }
    tau.alphar <- pow(sigma.alphar, -2)
    sigma.alphar ~ dunif(0,2)
    tau.betar <- pow(sigma.betar, -2)
    sigma.betar ~ dunif(0,2)
    beta0 ~ dnorm(0,0.01)
    beta1 ~ dnorm(0,0.01)
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  if (addarma){
  cat("  
  # AR(1) model
  for (c in 1:C){
   delta.ct[c,1] ~ dnorm(0, tau.stat.ar.c[c])
 tau.stat.ar.c[c] <- (1-pow(rho,2))/pow(exp(logsigma.ar.ct[c,1]),2)
   for (t in 2:nyears){
    delta.ct[c, t] ~ dnorm(rho*delta.ct[c, t-1], tau.ar.ct[c,t])
    tau.ar.ct[c,t] <- pow(exp(logsigma.ar.ct[c,t]),-2)
    #logsigma.ar.ct[c,t] <- minlogsd + betasd*totaldemand.ct[c,t]
   }
 for (t in 1:nyears){
    logsigma.ar.ct[c,t] <- minlogsd + betasd*(totaldemand.ct[c,t]-0.5)+
                              beta2sd*(totaldemand.ct[c,t]-0.5)^2
   }
}
minlogsd ~ dnorm(0, 0.1)
betasd ~ dnorm(0, 0.1)
beta2sd ~ dnorm(0, 0.1)
  # parameters of AR(1) model
  tau.ar <- pow(sigma.ar,-2)
  sigma.ar ~ dunif(0,2)
  rho ~ dunif(0,0.95)
 # tau.stat.ar <- (1-pow(rho,2))/pow(sigma.ar,2)
  
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  } else { # no arma
  cat("  
  #   delta.ct[1:C, 1:nyears] <- 0 # jags doesn't like this
  for (c in 1:C){
  for (t in 1:nyears){
     delta.ct[c,t] <- 0
  }
  }
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  } # and arma statement
  
  # add closing parentheses for the model
  cat("  
  }  #end model
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  return(modelpath)
  
} # end function