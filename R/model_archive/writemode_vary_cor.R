
WriteModelVaryCor <- function(
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
      yrepluup.i[i] ~ dnorm(pi.ct[getc.i[i], gett.i[i]], tau.y)
      }
      tau.y <- pow(.025, -2)
      #sigma.y ~ dunif(0, 2) # tried a prior for sigma and Rhat is 2.2
      ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  # process model (excl distortion term)
  cat(" 
      for (c in 1:C){
      a.c[c] <- alphabeta.c2[c,1]
      beta.c[c] <- alphabeta.c2[c,2]
      alphabeta.c2[c,1:2] ~ dmnorm(alphabeta.r2[getr.c[c], 1:2], InvSigma)

      for (t in 1:nyears){
      logit(pi.ct[c,t]) <- a.c[c] + demand.ct[c,t]*beta.c[c]
      }
      }

      
      # priors for cov matrix in C, alpha and beta
      rho.alphabeta ~ dunif(-0.95,1) # upper bound based on run w/o ar
      sigma.alpha ~ dunif(0,3)
      sigma.beta ~ dunif(0,5)
      #
      Sigma[1,1] <- pow(sigma.alpha,2)
      Sigma[2,2] <- pow(sigma.beta,2)
      Sigma[1,2] <- rho.alphabeta*sigma.beta*sigma.alpha
      Sigma[2,1] <- Sigma[1,2]
      InvSigma[1:2, 1:2] <- inverse(Sigma[, ])


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

  # add closing parentheses for the model
  cat("  
      }  #end model
      ", sep="", append = TRUE, file = modelpath, fill=TRUE)
  
  return(modelpath)
  
} # end function