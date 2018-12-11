
# plots for luup, ratio, lup, limiting ratio among users
# add obs for all? 
# eventually, now just lup and luup
percentiles = c(0.025, 0.5, 0.975)
lup.cqt <- luup.cqt <- ratio.cqt <-array(NA, c(C, 3, nyears))
for (t in 1:nyears){
  luup.cqt[,,t] <- mod$BUGSoutput$summary[paste("mu.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
  ratio.cqt[,,t] <- mod$BUGSoutput$summary[paste("ratiomu.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
  lup.cqt[,,t] <- mod$BUGSoutput$summary[paste("lupmu.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
}  


estyears <- seq(startyear, endyear)


pdf_name <- paste0(output.dir, "fig/countryest.pdf")
pdf(pdf_name, width = 14, height = 14)
# plot alphabetically
select_c <- order(name.c)
# or by region
#  select_c = which(getr.c==r) #which(is.element(name.c, selected_c)) #gets each countries iteration in our models loop of C from name.c
for (c in select_c){
  par(mfrow = c(2,2), lwd = 1.5, cex = 1.5, cex.lab=  1.5, cex.axis = 1.5)
  for (parname in c("luup", "ratio", "lup")){
    if (parname=="luup"){
      CIs.cqt = luup.cqt
      y.i <- luup.i
      ylab = "PDL"
    }
  if (parname=="lup"){
    CIs.cqt = lup.cqt
    y.i <- lup.i
    ylab = "PUL"
  }
  if (parname=="ratio"){
    CIs.cqt = ratio.cqt
    y.i <- ratio.i #rep(NA, n)
    ylab = "RD"
  }
  plot(1, type="n",
       xlim = range(estyears),
       ylab = ylab, xlab = "Time", 
       main = paste0(name.c[c], " (", regnames[getr.c[c]], ")"), 
       ylim = range(c(0, 1,CIs.cqt[c,,], (y.i[getc.i==c])), na.rm = T), 
       lwd = 5, pch = 19)
  abline(h=1)
  abline(h=0)
  AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = 2)
  lines(CIs.cqt[c,2,] ~ estyears, col = 2, lwd = 3)
  if (sum(getc.i==c)>0){
    points((y.i[getc.i==c]) ~ estyears[gett.i[getc.i==c]],lwd = 3, pch = 19,
           col = ifelse(is.element(which(getc.i==c), getitest), "grey", 2))
  }
  }
  plot(demand.ct[c,] ~ estyears, col = "green", type = "l", lwd= 3, ylab = "FPET estimates", xlab = "Time", ylim = c(0,1))
  lines(cpr.ct[c,]~ estyears, lwd= 3, col = 2)
  lines(c(demand.ct[c,] - cpr.ct[c,])~ estyears, lwd= 3, col = 4)
  legend("topleft", legend = c( "demand","CP", "unmet"), col = c("green",2,4), cex = 0.6, lty = 1, pch = -1)  
}
dev.off()

# parnames <- c("gamma", "timechange", "change")
# for (par in parnames){
#   print(round(quantile(c(mcmc.array[,,par]), percentiles),3))
# }
# round(quantile(1984+c(mcmc.array[,,"timechange"]), percentiles),3)
# # # plot CIs for selected parameters, using bayesplot
# mcmc_intervals(mcmc.array, pars = c("rho.alphabeta", "sigma.alpha", "sigma.beta"))
# mcmc_intervals(mcmc.array, pars = c("gamma", "timechange", "change"))
# # or with densities
# mcmc_areas(
#   x = mcmc.array,
#   pars =  c("rho.alphabeta", "sigma.alpha", "sigma.beta"),
#   prob = 0.8, # 80% intervals
#   prob_outer = 0.95, # 95%
#   point_est = "mean"
# )

