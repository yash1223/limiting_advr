
#For ratio model
#no intercept so -1
#get both predictors.i below
#cpr is contraceptive prevalance rate estimates
matrix1 <- 1 - (cpr.ct/demand.ct)**2 #demand not satisfied?
matrix2 <- (cpr.ct/demand.ct) - (cpr.ct/demand.ct)**2
matrix3 <- (cpr.ct/demand.ct)
matrix4 <- (cpr.ct/demand.ct)^2

predictor1.i <- rep(NA, n)
predictor2.i <- rep(NA, n)
predictor3.i <- rep(NA, n)
predictor4.i <- rep(NA, n)

for (i in 1:n) {
  predictor1.i[i]<-matrix1[getc.i[i],gett.i[i]]
  predictor2.i[i]<-matrix2[getc.i[i],gett.i[i]]
  predictor3.i[i]<-matrix3[getc.i[i],gett.i[i]]
  predictor4.i[i]<-matrix4[getc.i[i],gett.i[i]]
  
}

ratiodata <- cbind.data.frame(predictor1.i, predictor2.i, ratio.i)
fitratio <- lm(ratio.i~predictor1.i+predictor2.i-1,data=ratiodata)
plotdf$ratio_pred <- predict(fitratio, ratiodata)

fitratiolog <- lm(log(ratio.i)~predictor1.i+predictor2.i-1,data=ratiodata)

#testing new models
#first make a dataframe with variables to test
log_ratio <- log(ratio.i)
newmod_data<-data.frame(demand.i,factor(region.i),predictor1.i,predictor2.i,predictor3.i,predictor4.i,factor(year1),factor(year2),ratio.i,log_ratio,gett.i,factor(getc.i),ratio.i)

#basic models, simple version of LAs
fitratio <- lm(ratio.i~factor(region.i)+exp(predictor3.i)-1,data=newmod_data)
fitratiolog <- lm(log_ratio~factor(region.i)+exp(predictor3.i)-1,data=newmod_data)
#better performance than LA mod
#lets plot them quickly
plotdf$ratio_pred <- predict(fitratio, newmod_data)
plotdf$ratiolog_pred <- predict(fitratiolog, newmod_data)
#it would definitely be nice to have an intercept model although its not working with the freq setup
#lets try a mixed model to see how a hierachle intercept model would turn out
library(lme4)
require(MuMIn)
#r.squaredGLMM(fit) gets r2
#r.squaredLR(fit) gets r2

fit<-lmer(log(ratio.i)~poly(predictor3.i,2)+(1|getc.i)+(1|region.i),data=newmod_data)
fit<-lmer(log(ratio.i)~poly(1|predictor3.i,2)+(1|getc.i)+(1|region.i),data=newmod_data)
r.squaredGLMM(fit)
plotdf$ratiolog_pred <- predict(fit, newmod_data)


plot_customlm(plotname = "../PAA/fig/testfun6.pdf", data = plotdf, 
              country_column = "Country", 
              region_column = "Region", xvar = "demand_satisfied", yvar = "ratio_rd", yhat="ratio_pred",
              number_of_regions = R, region_names = regnames, region_vector = plotdf$Region)  


plotdf$ratiolog_pred <- predict(fitratiolog, ratiodata)

plot_customlm(plotname = "../PAA/fig/testfun7.3.pdf", data = plotdf, 
              country_column = "Country", 
              region_column = "Region", xvar = "demand_satisfied", yvar = "log_ratio", 
              yhat="ratiolog_pred",
              number_of_regions = R, region_names = regnames, region_vector = plotdf$Region)  



#test
ratiodata$region <- region.i
fitexp <- lm(ratio.i~factor(region)+predictor1.i+exp(predictor2.i),data=ratiodata)
plotdf$ratio_exp <- predict(fitexp, ratiodata)

plot_customlm(plotname = "../PAA/fig/logratioexplore_gregtest2.pdf", data = plotdf, 
              country_column = "Country", 
              region_column = "Region", xvar = "demand_satisfied", yvar = "ratio_rd", 
              yhat="ratio_exp",
              number_of_regions = R, region_names = regnames, region_vector = plotdf$Region)  











