CheckandfixIso <- function(varnames, touse = "iso", 
                           toreplace  = c("ISO.code")
                           # add any other labels that are used for iso that need to be replaced
){
  if(!is.element(touse, varnames)){
    varnames[is.element(varnames, toreplace)] <- touse
  } 
  if(!is.element(touse, varnames)){
    print("Warning: no iso codes found")
  }
  return(varnames)
}
# test
#varn <- CheckandfixIso(c("ISO.code", "x")); varn
#varn <- CheckandfixIso(c("y", "x")); varn
