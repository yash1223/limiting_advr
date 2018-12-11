
runs <- c("Run1_noar",  
          "run3_constant_noar",   "Run2_ar_final"  , "Run2_luupconstant_ar")
nruns <- length(runs)
runnamesnice <- c("nonconstant-noar",  "constant-noar", "nonconstant-ar","constant-ar")
do.validation <- FALSE
source("sub_readandprocessdata.R")
runname <- runs[1]
luup.i <- jagsdata$luup.i
lup.i <- jagsdata$lup.i
#ratio.i
getc.i <- jagsdata$getc.i
gett.i <- jagsdata$gett.i
residuals <- stresiduals <- list()
for (r in 1:nruns){
  runname <- runs[r]
  runnamenice <- runnamesnice[r]
  output.dir <- MakeDirs(runname)
  mod <- readRDS(file = paste0(output.dir, "mod.rds"))
  S <- length(mod$BUGSoutput$sims.matrix[,paste0("yrep", "luup", ".i[",1,"]")])
  #mcmc.array <- mod$BUGSoutput$sims.array
  resluup.i <- reslup.i <- resratio.i <- 
    stresluup.i <- streslup.i <- stresratio.i <- rep(NA, n)
  residuals[[runnamenice]] <- stresiduals[[runnamenice]] <-  list()
  for (i in 1:n){
    yrepluupi.s <- mod$BUGSoutput$sims.matrix[,paste0("yrep", "luup", ".i[",i,"]")]
    yreplupi.s <- mod$BUGSoutput$sims.matrix[,paste0("yrep", "lup", ".i[",i,"]")]
    yrepratioi.s <- GetRatio(luup = yrepluupi.s, lup = yreplupi.s, 
                             cpr = rep(cpr.i[i],S), demand = rep(demand.i[i],S))
    #yhatluupi.s <- c(mcmc.array[,, paste0("mu.ct[", getc.i[i], ",", gett.i[i], "]")])
    #yhatlupi.s <- c(mcmc.array[,, paste0("lupmu.ct[", getc.i[i], ",", gett.i[i], "]")])
    resluupi.s <- luup.i[i] - yrepluupi.s
    reslupi.s <- lup.i[i] - yreplupi.s
    resratioi.s <- ratio.i[i] - yrepratioi.s
    resluup.i[i] <- mean(resluupi.s)
    stresluup.i[i] <- resluup.i[i]/sd(resluupi.s)
    reslup.i[i] <- mean(reslupi.s)
    streslup.i[i] <- reslup.i[i]/sd(reslupi.s)
    resratio.i[i] <- mean(resratioi.s)
    stresratio.i[i] <- resratio.i[i]/sd(resratioi.s)
  }
  residuals[[runnamenice]][["luup"]] <- resluup.i
  residuals[[runnamenice]][["lup"]] <- reslup.i
  residuals[[runnamenice]][["ratio"]] <- resratio.i
  stresiduals[[runnamenice]][["luup"]] <- stresluup.i
  stresiduals[[runnamenice]][["lup"]] <- streslup.i
  stresiduals[[runnamenice]][["ratio"]] <- stresratio.i
}

year.i <- seq(startyear, endyear)[gett.i]

pdf_name <- paste0("fig/residuals.pdf")
pdf(pdf_name, width = 21, height = 14)
for (parname in c("luup", "lup", "ratio")){
  par(mfrow = c(2,4), lwd = 1.5, cex = 1.5, cex.lab=  1.5, cex.axis = 1.5)
  for (i in 1:2){
    for (pred in c("demand", "time")){
 
  if (i==1){ 
    restoplot = residuals
    resname <- "residuals"
  }
  if (i==2){
    restoplot = stresiduals
    resname <- "st. residuals"
  } 
  for (runname in names(restoplot)){
        if (pred=="demand") x.i = demand.i
        if (pred=="time") x.i = year.i
        plot(restoplot[[runname]][[parname]] ~ x.i, ylab = paste0(resname,"-", parname), xlab = pred, 
             col = getr.i, main = runname)
        curve(predict(loess(restoplot[[runname]][[parname]] ~ x.i),x), add = T, col = 2, lwd= 3)
        abline(h=0)
      }
    }
  }
}
dev.off()
