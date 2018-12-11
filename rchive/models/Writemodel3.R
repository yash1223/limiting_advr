
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
# to do: rewrite as inverse directly
#Sigma[1,1] <- pow(sigma.alpha,2)
#Sigma[2,2] <- pow(sigma.beta,2)
#      Sigma[1,2] <- rho.alphabeta*sigma.beta*sigma.alpha
#      Sigma[2,1] <- Sigma[1,2]
#      InvSigma[1:2, 1:2] <- inverse(Sigma[, ])
      rho.alphabeta ~ dunif(-1,1)
      sigma.alpha ~ dunif(0,2)
      sigma.beta ~ dunif(0,2)
tau.beta.conditional <- 1/(sigma.beta^2*(1-rho.alphabeta^2))
tau.alpha <- pow(sigma.alpha,2)

    for (c in 1:C){
      for (t in 1:nyears){
        logit(mu.ct[c, t]) <- logitpi.ct[c, t] + delta.ct[c,t]
        logitpi.ct[c,t] <- alpha.c[c] + beta.c[c]*logit(totaldemand.ct[c,t])
      }
#alpha.c[c] <- alphabeta.c2[c,1]
#beta.c[c] <- alphabeta.c2[c,2]
#      alphabeta.c2[c,1:2] ~ dmnorm(alphabeta.r2[getr.c[c], 1:2], InvSigma)
alpha.c[c] ~ dnorm(alphabeta.r2[r,1], tau.alpha)
beta.c[c] ~ dnorm(alphabeta.r2[r,2] - rho.alphabeta*sigma.beta/sigma.alpha*(alpha.c[c] - alphabeta.r2[r,2]),
                tau.beta.conditional)
} # end c loop
  for (r in 1:R){
    alphabeta.r2[r,1] ~ dnorm(beta0, tauc) # misuse of variable names
    alphabeta.r2[r,2] ~ dnorm(beta1, taur)
  }
  tauc <- pow(sigmac, -2)
  sigmac ~ dunif(0,2)
  taur <- pow(sigmar, -2)
  sigmar ~ dunif(0,2)
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
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
  rho <- 0.8#~ dunif(0.5,0.8)
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