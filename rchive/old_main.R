#--------
# main.R 
# model all proportions
# Leontine Alkema and Greg Guranich 
# April 2018
#--------
# estimating the prop of limiting among unmet 

# setup:
# main steps are in this master script
# project specific R functions are saved in the R subfolder 
# subscripts (that eventually are to be turned into R functions) are in sub_....R
# data are in data subfolder
# output folder contains output per model (by runname)
#----
# user/run specific info
# define WD
workdir <- getwd()
demandestimates.dir <- "data/demandnew.csv"
cprestimates.dir <- "data/cpr.csv"
#rawdata.dir <- "data/unmet_2017.csv"
#rawdata.dir <- "data/unmet2018.csv"
countryinfo.dir <- "data/Country-and-area-classification.csv"
# used for LUUP
rawdata_dhs.dir <- "data/dhs_2018_edit.csv"
rawdata_mics.dir <- "data/mics_2018_edit.csv"
fp20.dir<- "data/fp2020.csv"

runname <- "model2-full"
# runname <- "model2-SSA"
# runname <- "model2-SSA-2007"

do.validation <- F#TRUE#
addarma <- F#TRUE#

#---
# setting the workdir, sourcing functions
setwd(workdir)

Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
#Rfiles <- Rfiles[grepl(".R", Rfiles)] # select R scripts in that folder
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

# load libraries (different on server)
source("sub_loadpackages_G.R")

# make a output folder to store results from run
MakeDirs(runname)
output.dir <- MakeDirs(runname)

#---------------------------------------------
# reading data and processing
# some settings used throughout
startyear <- 1985 # later cause first dhs/mics obs in 1987 only
endyear <- 2020# 2030 # earlier end year for testing (less time to run jags)
nyears = endyear - startyear + 1
# startyear <- 1985
# endyear <- 2007
# nyears <- endyear - startyear - 1
SSA = FALSE
source("sub_readandprocessdata.R")
# inputs: filepaths to 3 inputs files
# outputs: jagsdata

#---------------------------------------------
# run a model
parnames <- c(
# model for luup  
    "alpha.c", "beta.c", "alphabeta.r2", "beta0", "beta1",
    "sigma.alpha", "sigma.beta", "rho.alphabeta",  "sigma.alphar", "sigma.betar", 
    "rho", "sigma.ar",
    "timechange","change", "gamma",

  # for ratio model
  "ralpha.c", "rbeta.c", "ralphabeta.r2", "rbeta0", "rbeta1",
  "rsigma.alpha", "rsigma.beta", "rrho.alphabeta",  "rsigma.alphar", "rsigma.betar", 
  "rrho", "rminlogsd", "rbetasd", "rbeta2sd",

# data models for luup and lup
# these are fixed now so no need to save
'selup.i',
'seluup.i',
#  "nonsamplsd", 
#  "lupnonsamplsd",


# and the indicators / results
"loglike.i",
"loglikelup.i",
  "lupmu.ct", 
  "mu.ct", 
"ratiomu.ct", 
"yrepluup.i",
"yreplup.i")#  , "loglike.i", 


 
#model.file <- WriteModel(output.dir, modelname = "model", addarma  = addarma)
#to run teh hacked model
#WriteModelHack
model.file <- WriteModel(output.dir, modelname = "model", addarma  = addarma)

#file.show(model.file)
mod <-jags.parallel(data = jagsdata,inits = inits(), #NULL,
                    parameters.to.save=parnames,
                    model.file = model.file,
                   n.chains=4, n.burnin = 2000, n.iter = 10000, n.thin = 8) 
#                   n.chains=4, n.burnin = 1000, n.iter = 3000, n.thin = 2) # 
#     n.chains=3, n.burnin = 500, n.iter = 1000, n.thin = 2) # test
#     n.chains=2, n.burnin = 5, n.iter = 10, n.thin = 1) # test

saveRDS(mod, file = paste0(output.dir, "mod.rds"))
# if you need to reload it:
#mod <- readRDS(file = paste0(output.dir, "mod.rds"))
mcmc.array <- mod$BUGSoutput$sims.array

#source("sub_convergence.R")
#source("sub_residuals.R")
#source("sub_timeplot.R")
#source("sub_validationoutput.R")



# The End!
