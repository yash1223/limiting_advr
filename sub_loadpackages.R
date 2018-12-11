# NAMESPACE ISSUES
# we had issues using ggplot and bayesplot in combination, use one at a time
# calling the namespace directly with double colon '::' is not elegant particularly for ploting packages

library("rjags")
library("R2jags")
library(tidyverse)
library(loo)
library(data.table)
# library(lme4)
# use bayesplot for model checking etc
# note that just installing bayesplot from cran didn't work for me, I had to get it from github
# you need to have devtools installed for that
# and some other dependencies may not work, eg first install 
#install.packages(c("reshape2", "ggridges", "rstanarm"))
#devtools::install_github("stan-dev/bayesplot", dependencies = TRUE, build_vignettes = TRUE)
#library(bayesplot)
library(ggplot2)
library(rstantools) # needed for PIT_loo
library(splines)
