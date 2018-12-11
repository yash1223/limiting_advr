
# sub_residuals

# estimates of intercepts and slopes
alphahat.c <- mod$BUGSoutput$summary[paste0("alpha.c[", seq(1,C), "]"), "50%"]
betahat.c <- mod$BUGSoutput$summary[paste0("beta.c[", seq(1,C), "]"), "50%"]
ralphahat.c <- mod$BUGSoutput$summary[paste0("ralpha.c[", seq(1,C), "]"), "50%"]
rbetahat.c <- mod$BUGSoutput$summary[paste0("rbeta.c[", seq(1,C), "]"), "50%"]

pdf_name <- paste0(output.dir, "fig/coeff.pdf")
pdf(pdf_name, width = 14, height = 8)
  par(mfrow =c(2,1))
  plot(alphahat.c~betahat.c, col = getr.c, pch = 19)
  text(alphahat.c~betahat.c, label = name.c)
  abline(h=0)
  abline(v=0)
  #legend("bottomleft", legend = regnames, col = seq(1,R), lty = -1, pch = 21)
  plot(ralphahat.c~rbetahat.c, col = getr.c, pch = 19)
  text(ralphahat.c~rbetahat.c, label = name.c)
  abline(h=0)
  abline(v=0)
dev.off()


# for luup
# use yreps instead
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
pdf_name <- paste0(output.dir, "fig/residuals.pdf")
pdf(pdf_name, width = 14, height = 14)
  par(mfrow =c(2,2))
  
  for (i in 1:2){
    if (i==1){
      res.i <- luup - yhatluup.i
      yhat.i <- yhatluup.i
      sigma.i <- sqrt(.0025^2 + seluup.i^2)# this is wrong, one 0 too many
      stdluupres.i <- (luup - yhatluup.i)/sigma.i
      
      ind <- "luup" 

    } else {
      res.i <- lup - yhatlup.i
      sigma.i <- sqrt(.0025^2 + selup.i^2)
      stdlupres.i <- (lup - yhatlup.i)/sigma.i
      yhat.i <- yhatlup.i
      ind <- "lup"    
    }
    plot(res.i~demand.i, col = getr.i, ylab = "ind")
    curve(predict(loess(res.i~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~yhat.i, ylab = "ind")
    curve(predict(loess(res.i~yhat.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(abs(res.i)~demand.i, ylab = "ind")
    curve(predict(loess(abs(res.i)~demand.i),x), add = T, col = 2, lwd= 3)
    plot(log(abs(res.i))~demand.i)
    curve(predict(loess(log(abs(res.i))~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~seq(startyear, endyear)[gett.i], ylab = "ind")
    curve(predict(loess(res.i~seq(startyear, endyear)[gett.i]),x), add = T, col = 2, lwd= 3)
    abline(h=0)
  } 
  for (i in 1:2){
    if (i==1){
      #res.i <- luup - yhatluup.i
      yhat.i <- yhatluup.i
      sigma.i <- sqrt(.0025^2 + seluup.i^2)
      res.i <- (luup - yhatluup.i)/sigma.i
      
      ind <- "luup" 
      
    } else {
      #res.i <- lup - yhatlup.i
      sigma.i <- sqrt(.0025^2 + selup.i^2)
      res.i <- (lup - yhatlup.i)/sigma.i
      yhat.i <- yhatlup.i
      ind <- "lup"    
    }
    plot(res.i~demand.i, col = getr.i, ylab = "ind", main="standardized residual plot")
    curve(predict(loess(res.i~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~yhat.i, ylab = "ind", main="standardized residual plot")
    curve(predict(loess(res.i~yhat.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(abs(res.i)~demand.i, ylab = "ind", main="standardized residual plot")
    curve(predict(loess(abs(res.i)~demand.i),x), add = T, col = 2, lwd= 3)
    plot(log(abs(res.i))~demand.i, main="standardized residual plot")
    curve(predict(loess(log(abs(res.i))~demand.i),x), add = T, col = 2, lwd= 3)
    abline(h=0)
    
    plot(res.i~seq(startyear, endyear)[gett.i], ylab = "ind", main="standardized residual plot")
    curve(predict(loess(res.i~seq(startyear, endyear)[gett.i]),x), add = T, col = 2, lwd= 3)
    abline(h=0)
  } 
dev.off()

#-------------
# rest is NOT updated!

# is this because the slopes are changing with time or because the slope is changing with demand?
# TBD (e.g. we're plotting residuals for last obs year and max demand, to then change the regression function)

# given these results, other diagnostics are going to show issues 
# but adding code here for illustration (to use eventually for better fitting models)

#----
# let's do an in-sample PIT
# this assumes all obs were included in the training set
pit.i <- rep(NA, n)
for (i in 1:n){
  yrepi.s <- mod$BUGSoutput$sims.matrix[,paste0("yrep.i[",i,"]")]#c(mcmc.array[,,paste0("yrep.i[",i,"]")])
  pit.i[i] <- mean(yrepi.s <= y.i[i])
} 
hist(pit.i, freq = F, xlab = "PIT-values", main = "In-sample fit")  # should look uniform
abline(h=1)
# here I added the curves as in the pit_loo hist from the bayesplot package
# but uniform density appr does not look good at boundaries so I wouldn't use it
#for (f in 1:30) lines(density(runif(n)),  col = "grey", lwd = 1)
# where do we get conservative bounds?
# plot PITs ~ yhat, predictors...
plot(pit.i ~ yhat.i)
curve(predict(loess(pit.i ~ yhat.i),x), add= T, col = 2, lw = 3)
abline(h=0.5)

plot(pit.i ~ demand.i)
curve(predict(loess(pit.i ~ yhat.i),x), add= T, col = 2, lw = 3)
abline(h=0.5)


plot(abs(pit.i-0.5) ~ yhat.i)
curve(predict(loess(abs(pit.i-0.5) ~ yhat.i),x), add= T, col = 2, lw = 3)
abline(h=0.25)

plot(abs(pit.i-0.5) ~ demand.i)
curve(predict(loess(abs(pit.i-0.5) ~ demand.i),x), add= T, col = 2, lw = 3)
abline(h=0.25)
# decreasing with demand..

plot(abs(pit.i-0.5) ~ gett.i)
curve(predict(loess(abs(pit.i-0.5) ~ gett.i),x), add= T, col = 2, lw = 3)
abline(h=0.25)
# decreasing recently

for (r in 1:R) hist(pit.i[getr.i==r])
#-----
# LOO using PSIS-LOO
loglike.sn <- mod$BUGSoutput$sims.list$loglike.i # simu x data points matrix with samples of logdensity
loo <- loo(loglike.sn)
pareto_k_table(loo) 
# influential points, to be expected given that we have country-spec parameters and 
# countries with only 1 or 2 observations
is.element(name.c[getc.i[which(loo$pareto_k >1)]], name.c[table(getc.i)<3])
# actually , 1 country with >2 obs is also influential
# I added a PIT based on the LOOs here (but wouldn't use it for this model, given issues with infl points)
# use psis here to save the weights needed for the PIT
if (packageVersion("loo") < "2.0.0") {
  psis1 <- psislw(-loglike.sn, cores = 2)
  lw <- psis1$lw_smooth
} else {
  psis1 <- psis(-loglike.sn, cores = 2)
  lw <- weights(psis1)
}
ppc_loo_pit_overlay(y = y.i, 
            yrep = mod$BUGSoutput$sims.matrix[,paste0("yrep.i[",seq(1,n),"]")],
            lw = lw)

?bayesplot
