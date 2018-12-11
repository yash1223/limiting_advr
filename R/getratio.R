

GetRatio <- function(luup, lup, cpr, demand){
  alllimiters <- demand*luup 
  allspacers <- demand - alllimiters
  unmet <- demand - cpr
  ul <- lup*unmet
  us <- unmet - ul
  ratio <- ((1-ul/alllimiters)/(1 - us/allspacers))
  return(ratio)
}

