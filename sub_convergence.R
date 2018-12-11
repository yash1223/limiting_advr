
mcmc.array <- mod$BUGSoutput$sims.array

max(mod$BUGSoutput$summary[, "Rhat"]) # not yet ok for final inference but ok for first checking
which.max(mod$BUGSoutput$summary[, "Rhat"])
mod$BUGSoutput$summary[mod$BUGSoutput$summary[, "Rhat"] >1.1,]
mod$BUGSoutput$summary[mod$BUGSoutput$summary[, "Rhat"] >1.5,]

# below is just misc code i've used for checking convergence in different parts of the model
pdf_name <- paste0(output.dir, "fig/trace.pdf")
pdf(pdf_name, width = 14, height = 14)
par(mfrow =c(2,2))
# checking some parameters
PlotTrace("timechange", mcmc.array)
PlotTrace("change", mcmc.array)
hist(c(mcmc.array[,,"timechange"]))
hist(c(mcmc.array[,,"change"]))
hist(c(mcmc.array[,,"gamma"])[c(mcmc.array[,,"change"])==1])

PlotTrace("rho.alphabeta", mcmc.array)
PlotTrace("sigma.alpha", mcmc.array)
PlotTrace("sigma.beta", mcmc.array)
PlotTrace("beta0", mcmc.array)
PlotTrace("beta1", mcmc.array)
#PlotTrace("nonsamplsd", mcmc.array)


#PlotTrace("lupnonsamplsd", mcmc.array)
PlotTrace("rrho.alphabeta", mcmc.array)
PlotTrace("rsigma.alpha", mcmc.array)
PlotTrace("rsigma.beta", mcmc.array)
PlotTrace("rbeta0", mcmc.array)
PlotTrace("rbeta1", mcmc.array)

PlotTrace("rsigma.alphar", mcmc.array)
PlotTrace("rsigma.betar", mcmc.array)

dev.off()

for (r in 1:R) PlotTrace(paste0("alphabeta.r2[", r, ",1]"), mcmc.array)
for (r in 1:R) PlotTrace(paste0("alphabeta.r2[", r, ",2]"), mcmc.array)

PlotTrace("rho", mcmc.array)
PlotTrace("sigma.ar", mcmc.array)
PlotTrace("minlogsd", mcmc.array)
PlotTrace("betasd", mcmc.array)

PlotTrace("sigma.alphar", mcmc.array)
PlotTrace("sigma.betar", mcmc.array)
for (c in 1:10) PlotTrace(paste0("rbeta.c[", c, "]"), mcmc.array)
for (c in 1:15) PlotTrace(paste0("ralpha.c[", c, "]"), mcmc.array)


c=which(name.c=="Colombia")
name.c[c]
PlotTrace(paste0("beta.c[", c, "]"), mcmc.array)
PlotTrace(paste0("alpha.c[", c, "]"), mcmc.array)
PlotTrace(paste0("ralpha.c[", c, "]"), mcmc.array)
PlotTrace(paste0("rbeta.c[", c, "]"), mcmc.array)
rationew <- function(a,b,d) a+b*d-(a+b)*d^2
curve(rationew(2,5,x), xlim = c(0,1))
curve(rationew(2,-1,x), add= T)
t <- 2015 - startyear+1
cpr.ct[c,t]
demand.ct[c,t]
cpr.ct[c,t]/demand.ct[c,t]
