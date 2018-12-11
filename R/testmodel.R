
WriteModelTest <- function(
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
    luup.i[i] ~ dnorm(pi.ct[getc.i[i], gett.i[i]], tau.y)
   }
   tau.y <- pow(.025, -2)
   #sigma.y ~ dunif(0, 2) # tried a prior for sigma and Rhat is 2.2
    ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 
  for (c in 1:C){
   a.c[c] ~ dnorm(a.r[getr.c[c]], tauc)
   beta1[c] ~ dnorm(0,.01)
   for (t in 1:nyears){
    logit(pi.ct[c,t]) <- a.c[c] + demand.ct[c,t]*beta1[c]
  }}
  for (r in 1:R){
   a.r[r] ~ dnorm(beta0, taur)
  }
  tauc <- pow(sigmac, -2)
  sigmac ~ dunif(0,2)
  taur <- pow(sigmar, -2)
  sigmar ~ dunif(0,2)
  beta0 ~ dnorm(0,0.01)
  # A ~ dunif(0,.5)
  # B ~ dunif(.5,1)    
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)

  # add closing parentheses for the model
  cat("  
  }  #end model
  ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  return(modelpath)
  
} # end function