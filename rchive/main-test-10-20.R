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

runname <- "model2_spline00"
# runname <- "model2-SSA-reg"
# runname <- "model2-full-reg"
# runname <- "model2-full"
# runname <- "model2-SSA" 
# runname <- "model2-NOTSSA"

# runname <- "model2-2007"
# runname <- "model2-SSA-2007" 
# runname <- "model2-NOTSSA2007"

# Post 2007
# runname <- "model2-POST2007"
# runname <- "model2-POST2007-SSA"
# runname <- "model-POST2007-NOTSSA"
SSA <- FALSE
NOTSSA <- FALSE
startyear <- 1985
#startyear <- 2007
endyear <- 2020 #2020 #2030 # earlier end year for testing (less time to run jags)
nyears <- endyear - startyear + 1
do.validation <- FALSE
addarma <- FALSE

Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") 
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) 
source("sub_loadpackages_G.R")

MakeDirs(runname)
output.dir <- MakeDirs(runname)

source("sub_readandprocessdata.R")
parnames <- c("yrepluup.i","a.c","beta.c", "eta.j")

# if (SSA == TRUE) {
#   model.file <- WriteModel2SSA(output.dir, modelname = "model", addarma  = addarma)
# } else {
#   model.file <- WriteModel2(output.dir, modelname = "model", addarma  = addarma)
# }
model.file <- WriteModel2_splines02(output.dir, modelname = "model", addarma  = addarma)

file.show(model.file)
mod <-jags.parallel(data = jagsdata, inits = inits(), #NULL,
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

source("sub_EDA_preproc.R")



# The End!
