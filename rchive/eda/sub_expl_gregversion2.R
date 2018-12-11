           
library(ggplot2)
# la shorter:
getnc.i <- name.c[getc.i]               
getnr.i <- regnames[getr.i]
getnt.i <- seq(startyear, endyear)[gett.i]

# now bind vectors into dataframe so we can work with this in ggplot
y.i <- lup
# now bind vectors into dataframe so we can work with this in ggplot                     
temp <- cbind.data.frame(y.i, getnc.i, getnr.i, getnt.i)

# now lets give all of the columns in our dataframe recognizable names
names(temp)[names(temp)=="getnc.i"] <- "Country"
names(temp)[names(temp)=="getnt.i"] <- "year"
names(temp)[names(temp)=="y.i"] <- "LUP"
names(temp)[names(temp)=="getnr.i"] <- "Region"


#adding some variables
plotdf <- temp
plotdf$demand <- demand.i

#LA let's call it logratio to avoid confusion
plotdf$ratio_rd <-(ratio.i) # also defined somewhere in preproc?
plotdf$demand_satisfied <- ds.i# demand satisfied .. should be defined in preproc somwhere
luup.i <- luup
plotdf$logit_luup <- logit(luup.i)
plotdf$luup <- (luup.i)
plotdf$ratio <- ratio.i
plotdf$cpr <- cpr.i
plotdf$log_ratio <- log(ratio.i)
# change the name of this region to better reflect what it is
levels(plotdf$Region)[levels(plotdf$Region)=="Africa"] <- "Sub-Saharan Africa"
levels(regnames)[levels(regnames)=="Africa"] <- "Sub-Saharan Africa"
plotdf$LUP <- as.numeric(as.character(plotdf$LUP))
plotdf$LUP_logit <- logit(plotdf$LUP)
#plotdf <- subset(plotdf, is.element(plotdf$Country, fp20$fp2020))
plotdf$logit_demand <- logit(plotdf$demand)

#latest demand recorded(most recent)
library(dplyr)
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
temp <- temp[c("Country","year","demand")]
colnames(temp) <- c("Country","latest_year","demand_ly")
plotdf <- merge(plotdf, temp, by = 'Country')
# logit transform
plotdf$logit_demand_ly <- logit(plotdf$demand_ly)


# Lets look at only max luup (similar to when recording amount of chemical found in water)
temp <- plotdf %>% group_by(Country) %>% filter(luup == max(luup))
ggplot(data=temp, aes(x=demand, y=luup)) + geom_point() + geom_smooth()
# Lets look at only max year (possibly most accurate?)
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
ggplot(data=temp, aes(x=demand, y=luup)) + geom_point() + geom_smooth()

# Lets get the difference between the max year and min year for each country
# This will give us the change in variables over respective records
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
temp2 <- plotdf %>% group_by(Country) %>% filter(year == min(year))
temp$pdl_change <- temp$luup - temp2$luup
temp$demand_change <- temp$demand - temp2$demand
range(temp$pdl_change)
ggplot(data=temp, aes(x=demand_change, y=pdl_change)) + geom_point() + geom_smooth()

# Lets get the change between subsequent observations for each country
temp <- plotdf %>%
  group_by(Country) %>%
  mutate(Diff_pdl = luup - lag(luup))
temp <- temp %>%
  group_by(Country) %>%
  mutate(Diff_demand = demand - lag(demand))
temp$Diff_pdl[is.na(temp$Diff_pdl)] <- 0
temp$Diff_demand[is.na(temp$Diff_demand)] <- 0
ggplot(data=temp, aes(x=Diff_demand, y=Diff_pdl)) + geom_point() + geom_smooth()

# adjusting for year
temp <- plotdf %>%
  group_by(Country) %>%
  mutate(Diff_pdl = (luup - lag(luup))/(year-lag(year)))
temp <- temp %>%
  group_by(Country) %>%
  mutate(Diff_demand = (demand - lag(demand))/(year-lag(year)))
temp$Diff_pdl[is.na(temp$Diff_pdl)] <- 0
temp$Diff_demand[is.na(temp$Diff_demand)] <- 0
ggplot(data=temp, aes(x=Diff_demand, y=Diff_pdl)) + geom_point() + geom_smooth()

# Same stuff for LUP
# Lets get the difference between the max year and min year for each country
# This will give us the change in variables over respective records
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
temp2 <- plotdf %>% group_by(Country) %>% filter(year == min(year))
temp$pdl_change <- temp$LUP - temp2$LUP
temp$demand_change <- temp$demand - temp2$demand
range(temp$pdl_change)
ggplot(data=temp, aes(x=demand_change, y=pdl_change, color=Region)) + 
  geom_point() + 
  geom_smooth(method = "loess", size =1, lty = 2, colour="black")



#sometimes this works if bayesplot has already been loaded, its preferable to restart and and only source(load_packages_G.r) in main script
#detach("package:bayesplot", unload=TRUE)
#library(ggplot2)

# Lets plot logit(PDL) against demand fig 3 4/22 PAA paper
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/pdl_logit_9-23_fp20.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "logit_luup",
                                xlab="Demand", ylab="logit(PDL)", region_column="Region")

# Lets plot PDL
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/pdl_9-23_fp20.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "luup",
                                xlab="Demand", ylab="PDL", region_column="Region") 
# Lets plot PDL (latest year version)
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/pdl_9-30_ly.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand_ly", yvar = "luup",
                                xlab="Demand", ylab="PDL", region_column="Region") 
# Lets plot PDL (latest year version) logit
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/pdl_9-30_ly_logit.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand_ly", yvar = "logit_luup",
                                xlab="Demand", ylab="logit PDL", region_column="Region") 

# Lets plot PDL (latest year version) logit logit
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/pdl_9-30_ly_logitlogit.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "logit_demand_ly", yvar = "logit_luup",
                                xlab="logit Demand", ylab="logit PDL", region_column="Region")

# Lets plot lup
plot_country_branches_editscale(plotname = paste0(getwd(), "/fig/lup_9-23_fp20.pdf"), 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "LUP",
                                xlab="Demand", ylab="LUP", region_column="Region") 

#Lets plot RD
plot_country_branches_editscale(plotname = "../PAA/fig/RDexplore_g1.pdf", 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "ratio",
                                xlab="Demand", ylab="RD", region_column="Region") 

# Lets plot logRD
plot_country_branches_editscale(plotname = "../PAA/fig/RDlogexplore_g1.pdf", 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "log_ratio",
                                xlab="Demand", ylab="log(RD)", region_column="Region") 

plot_country_branches_editscale(plotname = "../PAA/fig/RDlogexplore_ds.pdf", 
                                data = plotdf, country_column = "Country", number_of_regions=R, 
                                region_names = regnames, region_vector = plotdf$Region, 
                                xvar = "demand_satisfied", yvar = "log_ratio",
                                xlab="Demand satisfied", ylab="log(RD)", region_column="Region") 

#ratio
plot_regional_loess(plotname = paste0(getwd(), "/fig/ratio_cpr_9-23_fp20.pdf"), 
                     data = plotdf, country_column = "Country", number_of_regions=R, 
                     region_names = regnames, region_vector = plotdf$Region, xvar = "cpr", yvar = "ratio",
                     xlab="cpr", ylab="ratio", region_column="Region")


plot_regional_loess(plotname = paste0(getwd(), "/fig/ratio_demandsat_9-23_fp20.pdf"), 
                    data = plotdf, country_column = "Country", number_of_regions=R, 
                    region_names = regnames, region_vector = plotdf$Region, xvar = "demand_satisfied", yvar = "ratio",
                    xlab="demand satisfied", ylab="ratio", region_column="Region") 

plot_regional_loess(plotname = paste0(getwd(), "/fig/ratio_demand_9-23_fp20.pdf"), 
                    data = plotdf, country_column = "Country", number_of_regions=R, 
                    region_names = regnames, region_vector = plotdf$Region, xvar = "demand", yvar = "ratio",
                    xlab="demand", ylab="ratio", region_column="Region") 

oneplot <- rep(0, nrow(plotdf))
oneplot[plotdf$Region=='Sub−Saharan Africa' | plotdf$Region=='Asia'] <- 1
plot_one_loess(plotname = paste0(getwd(), "/fig/ratio_demandsat_subset_9-23_fp20.pdf"), 
                    data = plotdf, country_column = "Country", number_of_regions=R, 
                    region_names = regnames, region_vector = plotdf$Region, xvar = "demand_satisfied", yvar = "ratio",
                    xlab="demand satisfied", ylab="ratio", region_column="Region") 


###############################################################
# RESIDUALS AND YHATS
####################### Don't run this block unless you want a different model / directory ################
#Change wd to output plots
runname <- "run1_noar_validation"
setwd(workdir)
output.dir <- MakeDirs(runname)


# code to load in model files
mod <- readRDS(paste0("C:/Users/greg/Dropbox/unmet_greg/model_combi/output/",runname,"/mod.rds"))
mcmc.array <- mod$BUGSoutput$sims.array

##########################################################################################################

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


res.i <- luup - yhatluup.i
plotdf$predicted <- yhatluup.i
plotdf$residuals <- res.i
plotdf$luup <- luup
plotdf$residuals_abs <- abs(res.i)*100

pdf_name <-  "../PAA/fig/residual_run1.pdf"
pdf(pdf_name, width = 14, height = 8)

ggplot(plotdf, aes(x = demand, y = luup)) +
  ggtitle("Residuals*100") +
  geom_segment(aes(xend = demand, yend = predicted), alpha = .2) +
  geom_point(aes(color = factor(Region), size=plotdf$residuals_abs)) +
  guides(size= FALSE) +
  geom_point(aes(y = predicted), shape = 1)

ggplot(plotdf, aes(x = demand, y = luup)) +
  ggtitle("Residuals*100") +
  geom_segment(aes(xend = demand, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals), size=plotdf$residuals_abs) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(size= FALSE) + 
  geom_point(aes(y = predicted), shape = 1)
 # geom_text(data=subset(plotdf, residuals_abs > 4),
 #           aes(demand,luup,label=Region),vjust=-1.5)
 
dev.off()











#For ratio model
#no intercept so -1
#get both predictors.i below
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











