
FixIsoName <- function(varnames, wrongisonames = c("ISO.Code", "Iso.Code", "Iso.code", "Numerical.code")){
  if (sum(is.element(varnames, wrongisonames))>0){
    varnames[is.element(varnames,  wrongisonames)] <- "ISO.code"
  }
  return(varnames)
}