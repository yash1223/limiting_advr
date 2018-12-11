inits <- function(AddARMA = TRUE,  meta){
  initslist <- list()
  # if (AddARMA){
  #   initslist <- c(initslist, list("phi" = runif(1,0.6,0.8)))
  # }
  # make sure we keep x4 postive  for things to run...
  # for starters, make luup really small
  # on logitscale, so intercept  = InvLogit(0.05) = 0.5 and beta = 0 and gamma = 0
  # note that this means that checking convergence may be a challenge
  initslist <- c(initslist, list(alphabeta.c2 = cbind(rep(logit(0.05), C),rep(0,C)), gamma = 0)) 
  return(list(initslist))
} # end inits function
