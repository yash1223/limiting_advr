
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
   for (i in 1:n){
    y.i[i] ~ dnorm(mu.ct[getc.i[i], gett.i[i]], tau.y)

    #yrep.i[i] ~ dnorm(mu.ct[getc.i[i], gett.i[i]], tau.y)
   }
   tau.y <- pow(.025, -2)
   # LA increased the error variance on the ys (it may be more plausible)
   #sigma.y ~ dunif(0, 2) # tried a prior for sigma and Rhat is 2.2
    ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 
  taulogitp <- pow(sigmalogitp, -2)
  sigmalogitp ~ dunif(0,2)
  for (c in 1:C){
    logitpi.ct[c,t2000] ~ dnorm(a.r[getr.c[c]], tauc)
     for (t in (t2000+1):nyears){
      logitpi.ct[c, t] ~ dnorm(logitpi.ct[c,t-1] + beta.ct[c,t-1]*(totaldemand.ct[c,t] - totaldemand.ct[c,t-1]),
                                  taulogitp)
     }
     for (t in 1:(t2000-1)){
      logitpi.ct[c, t] ~ dnorm( (logitpi.ct[c,t+1] 
                              - beta.ct[c,t]*(totaldemand.ct[c,t+1] - totaldemand.ct[c,t])),
                              taulogitp)
                              

     }
    for (t in 1:nyears){
      logit(mu.ct[c, t]) <- logitpi.ct[c, t] #+ delta.ct[c,t]
#      beta.ct[c,t] <- beta1
#      beta.ct[c,t] <- weight1.ct[c,t]*beta.k[getk1.ct[c,t]] + (1-weight1.ct[c,t])*beta.k[getk2.ct[c,t]]
      mubeta.ct[c,t] <- weight1.ct[c,t]*beta.k[getk1.ct[c,t]] + (1-weight1.ct[c,t])*beta.k[getk2.ct[c,t]]
      
    }
    # (beta.ct - mubeta.ct) = elta.ct ~ ar(1)
   elta.ct[c,1] ~ dnorm(0, etau.stat.ar)
      for (t in 2:nyears){
      elta.ct[c, t] ~ dnorm(erho*elta.ct[c, t-1], etau.ar)
      }
for (t in 1:nyears){
    beta.ct[c,t] <- elta.ct[c,t] + mubeta.ct[c,t]
}
} # end c loop
      # parameters of AR(1) model
      etau.ar <- pow(esigma.ar,-2)
      esigma.ar ~ dunif(0,2)
      erho ~ dunif(0,1)
      etau.stat.ar <- (1-pow(erho,2))/pow(esigma.ar,2)
  # global betas (perhaps to make regional)
  beta.k[1] ~ dnorm(0,0.1)
  for (k in 2:K){
    beta.k[k] ~ dnorm(beta.k[k-1], taubeta)
  }
  taubeta <- pow(sigmabeta, -2)
  sigmabeta ~ dunif(0,2)


  for (r in 1:R){
   a.r[r] ~ dnorm(beta0, taur)
  }
  tauc <- pow(sigmac, -2)
  sigmac ~ dunif(0,2)
  taur <- pow(sigmar, -2)
  sigmar ~ dunif(0,2)
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  A ~ dunif(0,.5)
  B ~ dunif(.5,1)    
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  if (addarma){
  cat("  
  # AR(1) model
  for (c in 1:C){
   delta.ct[c,1] ~ dnorm(0, tau.stat.ar)
   for (t in 2:nyears){
    delta.ct[c, t] ~ dnorm(rho*delta.ct[c, t-1], tau.ar)
  }}
  # parameters of AR(1) model
  tau.ar <- pow(sigma.ar,-2)
  sigma.ar ~ dunif(0,2)
  rho ~ dunif(0,1)
  tau.stat.ar <- (1-pow(rho,2))/pow(sigma.ar,2)
  
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  } else { # no arma
  cat("  
for (c in 1:C){
for (t in 1:nyears){
   delta.ct[c,t] <- 0
}
}
#   delta.ct[1:C, 1:nyears] <- 0
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  } # and arma statement
  
  # add closing parentheses for the model
  cat("  
  }  #end model
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  return(modelpath)
  
} # end function