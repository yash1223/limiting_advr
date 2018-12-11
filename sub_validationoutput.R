
#sub_validationoutput.R

# indices of test set
getitest <- setdiff(seq(1,n), jagsdata$getitrain.j)
ntest <- length(getitest)
# errors
errors <- y.i[getitest] - mod$BUGSoutput$summary[paste0("yrep.i[",getitest,"]"), "50%"]
hist(errors)
mean(errors)  
median(errors)  
mean(abs(errors))
median(abs(errors))
#plot standardized residuals? see rchive pit code

plot(errors ~ demand.i[getitest])
abline(h=0)
# PIT
pit.j <- rep(NA, ntest)
for (j in 1:ntest){
  i <- getitest[j]
  yrepi.s <- mod$BUGSoutput$sims.matrix[,paste0("yrep.i[",i,"]")]
  pit.j[j] <- mean(yrepi.s <= y.i[i])
} 
hist(pit.j, freq = F, xlab = "PIT-values", main = "Predicting last obs")  # should look uniform
abline(h=1)

# coverage follows from pit
mean(pit.j < 0.05)
mean(pit.j > 0.95)
mean(pit.j < 0.1)
mean(pit.j > 0.9)

# plot over time
percentiles = c(0.025, 0.5, 0.975)
CIs.cqt <- array(NA, c(C, 3, nyears))
for (t in 1:nyears){
  #  CIs.cqt[,,t] <- InvLogit((mod$BUGSoutput$summary[paste("logitpi.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]))
  CIs.cqt[,,t] <- mod$BUGSoutput$summary[paste("mu.ct[", seq(1, C), ",", t, "]", sep = ""),paste(100*percentiles, "%", sep = "")]
}  
estyears <- seq(startyear, endyear)
pdf_name <- paste0("../fig/modelvalwithar.pdf")
pdf(pdf_name, width = 7, height = 7)
# plot alphabetically
select_c <- order(name.c)
# or by region
#  select_c = which(getr.c==r) #which(is.element(name.c, selected_c)) #gets each countries iteration in our models loop of C from name.c
for (c in select_c){
  plot(1, type="n",
       xlim = range(estyears),
       ylab = "LR", xlab = "Time", 
       main = name.c[c], 
       ylim = c(0, 1), 
       lwd = 5, pch = 19)
  AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = 2)
  lines(CIs.cqt[c,2,] ~ estyears, col = 2, lwd = 3)
  if (sum(getc.i==c)>0){
    points((y.i[getc.i==c]) ~ estyears[gett.i[getc.i==c]], lwd = 3, pch = 19, 
           col = ifelse(is.element(which(getc.i==c), getitest), "grey", 2))
  }
  # add demand
  lines(unlist(jagsdata$totaldemand.ct[c,]) ~ estyears, col = "green", lty = 2)
}
dev.off()
