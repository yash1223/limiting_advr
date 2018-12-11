
PlotCountries <- function(CIs,
                          pdf_name = NULL)#paste0("fig/compfull_val", runname,".pdf")
  {
# all other variables are assumed to be global for now 
pdf(pdf_name, width = 14, height = 14)
# plot alphabetically
select_c <- order(name.c)
# or by region
#  select_c = which(getr.c==r) #which(is.element(name.c, selected_c)) #gets each countries iteration in our models loop of C from name.c
for (c in select_c){
  par(mfrow = c(2,2), lwd = 1.5, cex = 1.5, cex.lab=  1.5, cex.axis = 1.5)
  for (parname in c("luup", "ratio", "lup")){
    if (parname=="luup"){
      CIspar = lapply(CIs, function(l) l$luup.cqt)
      y.i <- luup.i
      ylab = "PDL"
    }
    if (parname=="lup"){
      CIspar = lapply(CIs, function(l) l$lup.cqt)
      y.i <- lup.i
      ylab = "PUL"
    }
    if (parname=="ratio"){
      CIspar = lapply(CIs, function(l) l$ratio.cqt)
      y.i <- ratio.i #rep(NA, n)
      ylab = "RD"
    }
    plot(1, type="n",
         xlim = range(estyears),
         ylab = ylab, xlab = "Time", 
         main = paste0(name.c[c], " (", regnames[getr.c[c]], ")"), 
         ylim = range(c(0, 1,
                        #unlist(CIspar), # now not country specific
                        unlist(lapply(CIspar, function(l) l[c,,])), # now not country specific
                        (y.i[getc.i==c])), na.rm = T), 
         lwd = 5, pch = 19)
    abline(h=1)
    abline(h=0)
    for (r in 1:length(CIs)){
      CIs.cqt <- CIspar[[names(CIs)[r]]]
      AddCIs(CI.up.t = CIs.cqt[c,3,], CI.low.t = CIs.cqt[c,1,], seq.years.t = estyears, col = r)
    }
    # do point estimates last
    for (r in 1:length(CIs)){
      CIs.cqt <- CIspar[[names(CIs)[r]]]
      lines(CIs.cqt[c,2,] ~ estyears, col = r, lty = r, lwd = 3)
    }
    if (sum(getc.i==c)>0){
      points((y.i[getc.i==c]) ~ estyears[gett.i[getc.i==c]],lwd = 3, pch = 19,
             col = ifelse(is.element(which(getc.i==c), getitest), "grey", 2))
    }
  } # end par loop
  plot(demand.ct[c,] ~ estyears, col = "green", type = "l", lwd= 3, ylab = "FPET estimates", xlab = "Time", ylim = c(0,1))
  lines(cpr.ct[c,]~ estyears, lwd= 3, col = 2)
  lines(c(demand.ct[c,] - cpr.ct[c,])~ estyears, lwd= 3, col = 4)
  legend("topleft", legend = c( "demand","CP", "unmet"), col = c("green",2,4), cex = 0.6, lty = 1, pch = -1)  
  legend("bottomright", legend= names(CIs), col = seq(1, length(CIs)), lty = seq(1, length(CIs)), pch = -1)
  }
  dev.off()
  return()
}