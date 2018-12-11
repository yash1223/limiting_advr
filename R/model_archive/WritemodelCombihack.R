
WriteModelHack <- function(
  output.dir = getwd(), # to save models and their output in different dirs
  modelname = "model",
  addarma  = FALSE
){
  modelpath <- file.path(output.dir, paste0(modelname, 
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
    luup.i[getitrain.j[j]] ~ dnorm(mu.ct[getc.i[getitrain.j[j]], gett.i[getitrain.j[j]]], tauy.i[getitrain.j[j]])
    lup.i[getitrain.j[j]] ~ dnorm(lupmu.ct[getc.i[getitrain.j[j]], gett.i[getitrain.j[j]]], luptauy.i[getitrain.j[j]])
   }
   for (i in 1:n){
    tauy.i[i] <- 1/(seluup.i[i]^2 + nonsamplsd^2)
    luptauy.i[i] <- 1/(selup.i[i]^2 + lupnonsamplsd^2)
    # for checks
    yrepluup.i[i] ~ dnorm(mu.ct[getc.i[i], gett.i[i]], tauy.i[i])
    yreplup.i[i] ~ dnorm(lupmu.ct[getc.i[i], gett.i[i]], luptauy.i[i])

    loglike.i[i] <- logdensity.norm(luup.i[i], mu.ct[getc.i[i], gett.i[i]], tauy.i[i])
    loglikelup.i[i] <- logdensity.norm(lup.i[i], lupmu.ct[getc.i[i], gett.i[i]], luptauy.i[i])
   }
   nonsamplsd <- 0.025#~ dunif(0,0.1)
   lupnonsamplsd <- 0.025#~ dunif(0,0.1)
    ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 

    # luup
    for (c in 1:C){
      for (t in 1:nyears){
        logit(mu.ct[c, t]) <- logitpi.ct[c, t] + delta.ct[c,t]
        logitpi.ct[c,t] <- alpha.c[c] #+ beta.c[c]*demand.ct[c,t] + 
                       # gamma*change* (t > timechange)*log(max(t, timechange+1)-timechange) 
      }
      alpha.c[c] <- alphabeta.c2[c,1]
      beta.c[c] <- alphabeta.c2[c,2]
      alphabeta.c2[c,1:2] ~ dmnorm(alphabeta.r2[getr.c[c], 1:2], InvSigma)
      } # end c loop
      change ~ dbern(0.5)
      timechange ~ dunif(0,26)
      gamma ~ dunif(-0.5,0)

      Sigma[1,1] <- pow(sigma.alpha,2)
      Sigma[2,2] <- pow(sigma.beta,2)
      Sigma[1,2] <- rho.alphabeta*sigma.beta*sigma.alpha
      Sigma[2,1] <- Sigma[1,2]
      InvSigma[1:2, 1:2] <- inverse(Sigma[, ])
      # priors for cov matrix
      rho.alphabeta ~ dunif(-0.95,1) # upper bound based on run w/o ar
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

    # ratio
    for (c in 1:C){
      for (t in 1:nyears){
       ratiomu.ct[c,t] <- exp(logratiomu.ct[c, t])
       logratiomu.ct[c, t] <- rlogpi.ct[c, t] + rdelta.ct[c,t]
       rlogpi.ct[c, t] <- (ralpha.c[c] 
                            + rbeta.c[c]*cpr.ct[c,t]/demand.ct[c,t] 
                        -(ralpha.c[c]+rbeta.c[c])*pow(cpr.ct[c,t]/demand.ct[c,t],2))
#       rlogpi.ct[c, t] <- ralpha.c[c] + rbeta.c[c]*demand.ct[c,t]
      }
      ralpha.c[c] <- ralphabeta.c2[c,1]
      rbeta.c[c] <- ralphabeta.c2[c,2]
      ralphabeta.c2[c,1:2] ~ dmnorm(ralphabeta.r2[getr.c[c], 1:2], rInvSigma)
    } # end c loop
    rSigma[1,1] <- pow(rsigma.alpha,2)
    rSigma[2,2] <- pow(rsigma.beta,2)
    rSigma[1,2] <- rrho.alphabeta*rsigma.beta*rsigma.alpha
    rSigma[2,1] <- rSigma[1,2]
    rInvSigma[1:2, 1:2] <- inverse(rSigma[, ])
    # priors for cov matrix
    rrho.alphabeta ~ dunif(-0.95,1)
    rsigma.alpha ~ dunif(0,3)
    rsigma.beta ~ dunif(0,5)
    for (r in 1:R){
      ralphabeta.r2[r,1] ~ dnorm(rbeta0, rtau.alphar)
      ralphabeta.r2[r,2] ~ dnorm(rbeta1, rtau.betar)
    }
    rtau.alphar <- pow(rsigma.alphar, -2)
    rsigma.alphar ~ dunif(0,2)
    rtau.betar <- pow(rsigma.betar, -2)
    rsigma.betar ~ dunif(0,2)
    rbeta0 ~ dnorm(0,0.01)
    rbeta1 ~ dnorm(0,0.01)

  # combining ratio and luup to get lup (and positive props)
for (c in 1:C){
 for (t in 1:nyears){
  q.ct[c,t] <- cpr.ct[c,t]/demand.ct[c,t]
  p.ct[c,t] <- mu.ct[c,t]#*demand.ct[c,t]
# x1=cl
  x1.ct[c,t] <- ratiomu.ct[c,t]*q.ct[c,t]*p.ct[c,t]/(1-p.ct[c,t]+ratiomu.ct[c,t]*p.ct[c,t])
# x2 = ul
  x2.ct[c,t] <- p.ct[c,t] - x1.ct[c,t]
# x3 = cs
  x3.ct[c,t] <- q.ct[c,t] - x1.ct[c,t]
  x4.ct[c,t] <- 1-x1.ct[c,t]-x2.ct[c,t]-x3.ct[c,t]
  lupmu.ct[c,t] <- x2.ct[c,t]/(x2.ct[c,t]+x4.ct[c,t])
  input1.ct[c,t] ~ dinterval(x4.ct[c,t], vec)
}}
vec[1] <- 0.0001 
vec[2] <- 0.9999


  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  if (addarma){
  cat("  
  # AR(1) model for luup
  for (c in 1:C){
   delta.ct[c,1] ~ dnorm(0, tau.stat.ar)
   for (t in 2:nyears){
    delta.ct[c, t] ~ dnorm(rho*delta.ct[c, t-1], tau.ar)
   }
  }
  # parameters of AR(1) model
  tau.ar <- pow(sigma.ar,-2)
  sigma.ar ~ dunif(0,2)
  rho ~ dunif(0,0.95)
  tau.stat.ar <- (1-pow(rho,2))/pow(sigma.ar,2)

  # AR(1) model for ratio
  for (c in 1:C){
    rdelta.ct[c,1] ~ dnorm(0, rtau.stat.ar.c[c])
    rtau.stat.ar.c[c] <- (1-pow(rrho,2))/pow(exp(rlogsigma.ar.ct[c,1]),2)
    for (t in 2:nyears){
      rdelta.ct[c, t] ~ dnorm(rrho*rdelta.ct[c, t-1], rtau.ar.ct[c,t])
      rtau.ar.ct[c,t] <- pow(exp(rlogsigma.ar.ct[c,t]),-2)
    }
    for (t in 1:nyears){
      rlogsigma.ar.ct[c,t] <- rminlogsd + rbetasd*(demand.ct[c,t]-0.5)+
              rbeta2sd*(demand.ct[c,t]-0.5)^2
    }
  }
  rminlogsd ~ dnorm(0, 0.1)
  rbetasd ~ dnorm(0, 0.1)
  rbeta2sd ~ dnorm(0, 0.1)
  rrho ~ dunif(0,0.95)

  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  } else { # no arma
  cat("  
  #   delta.ct[1:C, 1:nyears] <- 0 # jags doesn't like this
  for (c in 1:C){
  for (t in 1:nyears){
     delta.ct[c,t] <- 0
     rdelta.ct[c,t] <- 0
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