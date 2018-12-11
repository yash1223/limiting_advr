
GetQudr <- function(a,b, ds) a*(1-ds^2) + b*(ds-ds^2)
curve(GetQudr(a=1,b=1,x), xlim = c(0,1))
curve(GetQudr(a=10,b=1,x), xlim = c(0,1))

y.i <- ratio.i
#pdf("../PAA/fig/ratioexplore.pdf", width = 9, height = 6)
#par(cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)
plot(y.i ~ ds.i, col = getr.i, log = "y", pch = 19, ylab = "ratio", xlab = "demand satisfied")
abline(h=1)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(ds.i[select])]
  lines((y.i[select2]) ~ ds.i[select2], col= getr.c[c])
} 
legend("bottomright", legend = regnames, col = seq(1,R), cex = 0.6, pch = -1, lty = 1)
#dev.off()

#edit from greg
#this is what i have for luup in preproc
#y.i <- logit(luup)
#y.i <- logit(luup.i)

y.i <- ratio.i
#pdf("../PAA/fig/luupexplore_gregedit2.pdf", width = 9, height = 6)
#par(cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)
plot(y.i ~ demand.i, col = getr.i,  pch = 19, ylab = "logit(limiting out of demand(LoD))", xlab = "demand")
#abline(h=1)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines((y.i[select2]) ~ demand.i[select2], col= getr.c[c])
} 
legend("topleft", legend = regnames, col = seq(1,R), cex = 0.6, pch = -1, lty = 1)
#dev.off()

# ratio =1 when ds = 1
# could do a quadr fit with that

y.i <- ratio.i
select <- which(y.i > 1)
plot(log(y.i[select]-1) ~ demand.i[select], col = getr.i)
for (c in unique(getc.i[select])){
  select1 <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- select1[order(demand.i[select][select1])]
  lines(log(y.i[select][select2]-1) ~ demand.i[select][select2], col= getr.c[c])
} 
plot(log(y.i[select]-1) ~ gett.i[select], col = getr.i)
for (c in unique(getc.i[select])){
  select1 <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- select1[order(gett.i[select1])]
  lines(log(y.i[select][select2]-1) ~ gett.i[select][select2], col= getr.c[c])
} 
i <- which(y.i < 0.22 & demand.i > 0.38)
i <- which(y.i < 0.05)
name.c[getc.i[i]]

# spec for ratio
which(y.i==0)
name.c[getc.i[which(y.i==0)]]

plot(y.i ~ demand.i, col = getr.i, log = "y")
curve(predict(loess(y.i ~ demand.i),x), add= T, col = 2, lwd = 3)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(y.i[select2] ~ demand.i[select2], col= getr.c[c])
} 
abline(h=1)

####same thing with ratio >1
y.i <- ratio.i
select <- which(y.i > 1)
plot(y.i[select] ~ demand.i[select], col = getr.i, log = "y")
curve(predict(loess(y.i[select] ~ demand.i[select]),x), add= T, col = 2, lwd = 3)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(y.i[select2] ~ demand.i[select2], col= getr.c[c])
} 
abline(h=1)



#edit from greg
#this is what i have for luup in preproc
y.i <- logit(luup)
#y.i <- logit(luup.i)
y.i <- ratio

plot(logit(y.i) ~ demand.i, col = getr.i)
modlm <- lm(logit(y.i) ~ log(1+demand.i))
curve(modlm$coeff[1]+modlm$coeff[2]*log(1+x), add = T, col = 2, lwd = 2)

modlm <- lm(logit(y.i) ~ demand.i)
demand2.i <- demand.i^2
abline(modlm$coeff)
modlm2 <- lm(logit(y.i) ~ demand.i + demand2.i)
curve(modlm2$coeff[1]+modlm2$coeff[2]*x + modlm2$coeff[3]*x^2, add = T, col = 2, lwd = 2)
summary(modlm2)


# logit logit
plot(logit(y.i) ~ logit(demand.i), col = getr.i)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(logit(y.i)[select2] ~ logit(demand.i)[select2], col= getr.c[c])
} 
plot(logit(y.i) ~ (demand.i), col = getr.i)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(logit(y.i)[select2] ~ (demand.i)[select2], col= getr.c[c])
} 

c= 6
name.c[6]
getc.i== 6

modlm <- lm(logit(y.i) ~ logit(demand.i))
abline(modlm$coeff)
logitdemand2.i <- logit(demand.i)^2
modlm2 <- lm(logit(y.i) ~ logit(demand.i) + logitdemand2.i)
curve(modlm2$coeff[1]+modlm2$coeff[2]*x + modlm2$coeff[3]*x^2, add = T, col = 2, lwd = 2)
summary(modlm2)# now quadr term not sign! 
#just quadr
modlm2 <- lm(logit(y.i) ~  logitdemand2.i)
curve(modlm2$coeff[1]+ modlm2$coeff[2]*x^2, add = T, col = 3, lwd = 2)
summary(modlm2)# now quadr term not sign! 

plot(logit(y.i) ~ (demand.i), col = getr.i)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(logit(y.i)[select2] ~ (demand.i)[select2], col= getr.c[c])
} 
modlm2 <- lm(logit(y.i) ~  demand2.i)
curve(modlm2$coeff[1]+ modlm2$coeff[2]*x^2, add = T, col = 3, lwd = 2)
summary(modlm2)# now quadr term not sign! 


# so let's continue with this setup
# what about just regional dummies?
modreg <- lm(logit(y.i) ~ as.factor(getr.i))
summary(modreg) # R2 of 46%
modreglm <- lm(logit(y.i) ~ logit(demand.i) + as.factor(getr.i))
summary(modreglm) # R2 of 61%, some rregions different intercept
regnames
# interaction between slopes
# only sign for reg4, LAC (with much lower slope)
modreglm <- lm(logit(y.i) ~ logit(demand.i)*as.factor(getr.i))
summary(modreglm) # R2 of 65%
regnames[4]

# let's do an lmer fit to get country spec slopes
library(arm)
lmer()
m0 <- lmer (logit(y.i) ~  logit(demand.i) + (1+logit(demand.i)| getc.i) +  (1+logit(demand.i)| getr.i))
display(m0)

# no longer used
plot(y.i ~ demand.i, col = getr.i)
modlm <- lm(y.i ~ demand.i)
abline(modlm$coeff)
demand2.i <- demand.i^2
summary(modlm)
modlm2 <- lm(y.i ~ demand.i + demand2.i)
curve(modlm2$coeff[1]+modlm2$coeff[2]*x + modlm2$coeff[3]*x^2, add = T, col = 2, lwd = 2)
summary(modlm2) # r2 of 56%
# what about just regional dummies?
modreg <- lm(y.i ~ as.factor(getr.i))
summary(modreg) # R2 of 47%

modreglm <- lm(y.i ~ as.factor(getr.i)+demand.i + demand2.i)
summary(modreglm) # R2 of 65%

regnames
name.c[getr.c==4]
name.c[getc.i[getr.i==4]]

plot(logit(y.i) ~ demand.i, col = getr.i)
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(logit(y.i[select2]) ~ demand.i[select2], col= getr.c[c])
} 

mean(is.element(df$Source, c("DHS", "MICS")))
select <- which(is.element(df$Source, c("DHS", "MICS")) & getr.i==4)
plot(logit(y.i[select]) ~ demand.i[select], col = getr.i[select])
for (c in unique(getc.i[select])){
  selecta <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- selecta[order(demand.i[select][selecta])]
  lines(logit(y.i[select][select2]) ~ demand.i[select][select2], col= getr.c[c])
} 
plot(logit(y.i[select]) ~ logit(demand.i[select]), col = getr.i)
for (c in unique(getc.i[select])){
  selecta <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- selecta[order(demand.i[select][selecta])]
  lines(logit(y.i[select][select2]) ~ logit(demand.i[select][select2]), col= getr.c[c])
} 
plot((y.i[select]) ~ demand.i[select], col = getr.i[select])
for (c in unique(getc.i[select])){
  selecta <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- selecta[order(demand.i[select][selecta])]
  lines((y.i[select][select2]) ~ demand.i[select][select2], col= getr.c[c])
} 
plot(logit(y.i[select]) ~ gett.i[select], col = getr.i)
for (c in unique(getc.i[select])){
  selecta <- which(getc.i[select]==c)
  # order by demand.i
  select2 <- selecta[order(gett.i[select][selecta])]
  lines(logit(y.i[select][select2]) ~ gett.i[select][select2], col= getr.c[c])
} 


curve(InvLogit(0.5+1.5*sqrt(x)), xlim = c(0,1), col = 3)


curve(log(1+x), xlim = c(0,1), col = 3)
curve(sqrt(x)-1, add = T, col = 2)

curve(InvLogit(sqrt(x))-0.5,  col = 2)
abline(0,1)
curve(3*(InvLogit(log(1+x))-0.5), xlim = c(0,1))
abline(0,1)

curve(InvLogit(x)-0.5, add = T)

plot(y.i ~ gett.i, col = getr.i, log = "y")
for (c in unique(getc.i)){
  select <- which(getc.i==c)
  # order by demand.i
  select2 <- select[order(demand.i[select])]
  lines(y.i[select2] ~ gett.i[select2], col= getr.c[c])
} 

# check how much of an extrapolation is needed to get to 2030 (change in totaldemand)
lastobsdemand.c <- rep(NA, C)
for (c in 1:C) lastobsdemand.c[c] <- totaldemand.ct[c,max(gett.i[getc.i==c])]

plot(c(totaldemand.ct[,nyears] - lastobsdemand.c) ~ lastobsdemand.c)

i <- which(y.i < 0.22 & demand.i > 0.38)
i <- which(y.i < 0.05)

