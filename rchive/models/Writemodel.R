
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
   #sigma.y ~ dunif(0, 2) # tried a prior for sigma and Rhat is 2.2
    ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 
  for (c in 1:C){
   a.c[c] ~ dnorm(a.r[getr.c[c]], tauc)
   for (t in 1:nyears){
    #kappa.ct[c,t] <- (a.c[c] + logit(totaldemand.ct[c,t])*beta1)
    #pi.ct[c, t] <- (A+((B-A)/(1+exp(-(kappa.ct[c, t])))))
    kappa.ct[c,t] <- (a.c[c] + logit(totaldemand.ct[c,t])*beta1)
    pi.ct[c, t] <- (A+((B-A)/(1+exp(-(kappa.ct[c, t])))))
    logit(mu.ct[c, t]) <- logit(pi.ct[c, t]) + delta.ct[c,t]
  }}
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
  tau.stat <- (1-pow(rho,2))/pow(sigma.ar,2)
  }
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