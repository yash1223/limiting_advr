
#runs <- list.files(file.path(paste0(getwd(),"/Output/"))) 
#runs[-1]

runs <- c("Run1_noar",            "Run2_ar_final"    ,            
  "Run2_luupconstant_ar"     ,    
  "run3_constant_noar")
#,         
#  "Run3_luupconstant_noar") # the last two are the same runs? yes, confirmed in plots

runs <- c("Run1_noar",            "Run2_ar_again" )

runsval <- c(       "run1_noar_validation"   ,                 
          "run2_ar_validation"       ,     "run2_constant_ar_validation"  ,   
          "run3_constant_noar_validation" )

do.validation <- FALSE
source("sub_readandprocessdata.R")
runname <- runs[1]
luup.i <- jagsdata$luup.i
lup.i <- jagsdata$lup.i
getc.i <- jagsdata$getc.i
gett.i <- jagsdata$gett.i
getitest <- setdiff(seq(1,n), jagsdata$getitrain.j)
ntest <- length(getitest)
CIs <- list()
for (runname in runs){
  output.dir <- MakeDirs(runname)
  mod <- readRDS(file = paste0(output.dir, "mod.rds"))
  mcmc.array <- mod$BUGSoutput$sims.array
  source("sub_timeplot.R")
  CIs[[runname]] <- list(luup.cqt = luup.cqt, ratio.cqt = ratio.cqt, lup.cqt = lup.cqt)
}
#dev.off()
names(CIs)

do.validation <- TRUE
source("sub_readandprocessdata.R")
runname <- runsval[1]
CIsval <- list()
for (runname in runsval){
  output.dir <- MakeDirs(runname)
  mod <- readRDS(file = paste0(output.dir, "mod.rds"))
  mcmc.array <- mod$BUGSoutput$sims.array
  source("sub_timeplot.R")
  CIsval[[runname]] <- list(luup.cqt = luup.cqt, ratio.cqt = ratio.cqt, lup.cqt = lup.cqt)
}

# one overview plot with all runs
PlotCountries(CIs,pdf_name = paste0("fig/comp_withandwithoutar2.pdf"))
# two runs w/o ar
runsnoar <- c("Run1_noar",      
          "run3_constant_noar")
# there should be an easier way :)
tmp <-        list("Run1_noar" = CIs[["Run1_noar"]], "run3_constant_noar" = CIs[["run3_constant_noar"]])
#names(tmp)
#names(tmp[[1]])
PlotCountries(tmp,pdf_name = paste0("fig/compnoar.pdf"))
#dev.off()

# compare elpds for those 2 models? (but many large ks...)
# to do anyway: add direct comparison between nonconstant_nonar and constant_noar,
# same for ar


# calculate and compare validation results
do.validation <- TRUE
source("sub_readandprocessdata.R")
getitest <- setdiff(seq(1,n), jagsdata$getitrain.j)
ntest <- length(getitest)
resval <- list()
for (runname in runsval){
  output.dir <- MakeDirs(runname)
  mod <- readRDS(file = paste0(output.dir, "mod.rds"))
  #mcmc.array <- mod$BUGSoutput$sims.array
  pit.j <- error.j <- rep(NA, ntest)
  resval[[runname]] <- list()
  for (parname in c("luup", "lup")){
    if (parname=="luup"){
      y.i <- luup.i
      ylab = "PDL"
    }
    if (parname=="lup"){
      y.i <- lup.i
      ylab = "PUL"
    }
    for (j in 1:ntest){
      i <- getitest[j]
      yrepi.s <- mod$BUGSoutput$sims.matrix[,paste0("yrep", parname, ".i[",i,"]")]
      pit.j[j] <- mean(yrepi.s <= y.i[i])
      error.j[j] <- y.i[i] - median(yrepi.s)
    } 
    resval[[runname]][[parname]] <- data.frame(pit.j = pit.j, error.j = error.j)
  }
}
ressummall <- list()
for (runname in names(resval)) ressummall[[runname]] <- lapply(resval[[runname]], function(l) list(
  me = mean(l$error.j),  
  mede = median(l$error.j),
  mae = mean(abs(l$error.j)),  
  medae = median(abs(l$error.j)),
  below95 = mean(l$pit.j < 0.025),
  above95 = mean(l$pit.j > 0.975),
  below90 = mean(l$pit.j < 0.05),
  above90 = mean(l$pit.j > 0.95),
  below80 = mean(l$pit.j < 0.1),
  above80 = mean(l$pit.j > 0.9)
  ))
names(ressummall[[1]])
ressumm <- lapply(ressummall, function(l) l$luup)
# repeat for lup
ressumm <- lapply(ressummall, function(l) l$lup)
unlist(lapply(ressumm, function(l) l$me))
unlist(lapply(ressumm, function(l) l$mede))
unlist(lapply(ressumm, function(l) l$mae))
unlist(lapply(ressumm, function(l) l$medae))
unlist(lapply(ressumm, function(l) l$below95))
unlist(lapply(ressumm, function(l) l$above95))
unlist(lapply(ressumm, function(l) l$below90))
unlist(lapply(ressumm, function(l) l$above90))
unlist(lapply(ressumm, function(l) l$below80))
unlist(lapply(ressumm, function(l) l$above80))


hist(errors)
plot(errors ~ demand.i[getitest])
abline(h=0)
hist(pit.j, freq = F, xlab = "PIT-values", main = "Predicting last obs")  # should look uniform
abline(h=1)



  
