####################### Don't run this block unless you want a different model / directory ################
#Change wd to output plots
runname <- "run1_noar_validation"
setwd(workdir)
output.dir <- MakeDirs(runname)


# code to load in model files
mod <- readRDS(paste0("C:/Users/greg/Dropbox/unmet_greg/model_combi/output/",runname,"/mod.rds"))
##########################################################################################################



# loo stuff
loglike1.sn <- mod$BUGSoutput$sims.list$loglike.i
loglikelup1.sn <- mod$BUGSoutput$sims.list$loglikelup.i
logboth.sn <- (loglike1.sn+loglikelup1.sn)

#Run3_luupconstant_noar_val
loo1 <- loo(loglike1.sn)
loolup1 <- loo(loglikelup1.sn)
looboth1 <- loo(logboth.sn)
k_logic <- loo1$pointwise[,"p_loo"] > .5
k_logic_lup <- loolup1$pointwise[,"p_loo"] > .5

loodir <- paste0(output.dir,"fig/loo.txt")
sink(loodir)
print(loo1)
print(loolup1)
sink()

# for luup
yhatluup.i <- rep(NA, n)
for (i in 1:n) yhatluup.i[i] <- (quantile(c(mcmc.array[,,
                                                  paste0("mu.ct[", getc.i[i], ",", gett.i[i], "]")]), 0.5))

# for lup
yhatlup.i <- rep(NA, n)
for (i in 1:n) yhatlup.i[i] <- (quantile(c(mcmc.array[,,
                                                          paste0("lupmu.ct[", getc.i[i], ",", gett.i[i], "]")]), 0.5))


seluup.i <- rep(NA, n)
for (i in 1:n) seluup.i[i] <- quantile(c(mcmc.array[,,
                                                       paste0("seluup.i[",i,"]")]), 0.5)

selup.i <- rep(NA, n)
for (i in 1:n) selup.i[i] <- quantile(c(mcmc.array[,,
                                                    paste0("selup.i[",i,"]")]), 0.5)



# plot
pdf_name <- paste0(output.dir, "fig/residuals_k.pdf")
pdf(pdf_name, width = 14, height = 14)
  par(mfrow =c(2,2))
  
  for (i in 1:2){
    if (i==1){
      res.i <- luup - yhatluup.i
      yhat.i <- yhatluup.i
      ind <- "luup"
      temp = factor(k_logic)

    } else {
      res.i <- lup - yhatlup.i
      sigma.i <- sqrt(.0025^2 + selup.i^2)
      yhat.i <- yhatlup.i
      ind <- "lup"   
      temp = factor(k_logic_lup)
    }
    plot(res.i~demand.i, col = temp, ylab = paste0(ind))
    curve(predict(loess(res.i~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~yhat.i, col = temp, ylab = paste0(ind))
    curve(predict(loess(res.i~yhat.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(abs(res.i)~demand.i, col = temp, ylab = paste0(ind))
    curve(predict(loess(abs(res.i)~demand.i),x), add = T, col = 2, lwd= 3)
    plot(log(abs(res.i))~demand.i, col = temp, ylab = paste0(ind))
    curve(predict(loess(log(abs(res.i))~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~seq(startyear, endyear)[gett.i], col = temp, ylab = paste0(ind))
    curve(predict(loess(res.i~seq(startyear, endyear)[gett.i]),x), add = T, col = 2, lwd= 3)
    abline(h=0)
  } 
dev.off()
  
  
  
