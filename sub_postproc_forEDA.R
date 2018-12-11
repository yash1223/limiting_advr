getnc.i <- name.c[getc.i]               
getnr.i <- regnames[getr.i]
getnt.i <- seq(startyear, endyear)[gett.i]

y.i <- lup
PDL <- luup
logit_PDL <- logit(luup)
log_ratio <- log(ratio.i)

plotdf <- cbind.data.frame(y.i, PDL, getnc.i, getc.i, getnr.i, getnt.i, demand.i, ratio.i, log_ratio, ds.i, logit_PDL, cpr.i)
# now lets give all of the columns in our dataframe recognizable names
names(plotdf)[names(plotdf)=="getnc.i"] <- "Country"
names(plotdf)[names(plotdf)=="getnr.i"] <- "Region"
names(plotdf)[names(plotdf)=="getnt.i"] <- "year"
names(plotdf)[names(plotdf)=="y.i"] <- "LUP"
names(plotdf)[names(plotdf)=="demand.i"] <- "demand"
names(plotdf)[names(plotdf)=="ratio.i"] <- "ratio"
names(plotdf)[names(plotdf)=="ds.i"] <- "demand_satisfied"
names(plotdf)[names(plotdf)=="cpr.i"] <- "CP"


percentiles = c(0.025, 0.5, 0.975)
temp <- readRDS(file.path(output.dir, "mod.rds"))
mcmc.array <- temp$BUGSoutput$sims.array
summary <- temp$BUGSoutput$summary
yrep.i1 <- summary[paste0("yrepluup.i[",1:n,"]"), paste0(100*percentiles, "%")]
a.c <- summary[paste0("a.c[",1:C,"]"), "50%"]
beta.c <- summary[paste0("beta.c[",1:C,"]"), "50%"]
plot_jags_cor <- data.frame(a.c, beta.c)
plot_jags <- data.frame(yrep.i1, getnc.i, getnt.i, jagsdata$luup.i, getnr.i, demand.i)





########### BEYOND THIS POINT IS CODE TO BE ARCHIVED FROM EDA ##################

#lets see if we can get country-csv esque file from other leo repo, postpone for now
# temp <- read.csv(file.path(getwd(), "data","gdp.csv"))
# temp$Country.Name
# unique(birth_df$alpha.2)[!is.element(unique(plotdf$Country), unique(temp$Country.Name))]

# change the name of this region to better reflect what it is
# levels(plotdf$Region)[levels(plotdf$Region)=="Africa"] <- "Sub-Saharan Africa"
# levels(regnames)[levels(regnames)=="Africa"] <- "Sub-Saharan Africa"
# plotdf$LUP <- as.numeric(as.character(plotdf$LUP))
# plotdf$LUP_logit <- logit(plotdf$LUP)
# #plotdf <- subset(plotdf, is.element(plotdf$Country, fp20$fp2020))
# plotdf$logit_demand <- logit(plotdf$demand)
# 
# #latest demand recorded(most recent)
# temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
# temp <- temp[c("Country","year","demand")]
# colnames(temp) <- c("Country","latest_year","demand_ly")
# plotdf <- merge(plotdf, temp, by = 'Country')
# 
# #MINMAX
# temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
# temp2 <- plotdf %>% group_by(Country) %>% filter(year == min(year))
# temp$logit_PDL_change <- temp$logit_PDL - temp2$logit_PDL
# temp$demand_change <- temp$demand - temp2$demand
# temp <- temp[c("Country", "logit_PDL_change", "demand_change")]
# plotdf <- merge(plotdf, temp, by = 'Country')
# 
# #LAG #LAG ADJ LAG YEAR #AVG DEMAND
# plotdf <- plotdf %>%
#   group_by(Country) %>%
#   mutate(logit_PDL_lag = logit_PDL - lag(logit_PDL)) %>%
#   mutate(demand_lag = demand - lag(demand)) %>%
#   mutate(logit_PDL_lag_adj = (logit_PDL - lag(logit_PDL))/(year-lag(year))) %>%
#   mutate(demand_lag_adj = (demand - lag(demand))/(year-lag(year))) %>%
#   mutate(demand_avg = mean(demand))
# 
# plotdf$logit_PDL_lag[is.na(plotdf$logit_PDL_lag)] <- 0
# plotdf$demand_lag[is.na(plotdf$demand_lag)] <- 0
# plotdf$logit_PDL_lag_adj[is.na(plotdf$logit_PDL_lag_adj)] <- 0
# plotdf$demand_lag_adj[is.na(plotdf$demand_lag_adj)] <- 0
# 
# # JAGS estimates for betas without shrinkage w varrying intercept
# temp <- readRDS(file.path("/home/greggu/git/limiting_spacing_breakdown/output/vary_int", "mod.rds"))
# mcmc.array <- temp$BUGSoutput$sims.array
# summary <- temp$BUGSoutput$summary
# betac <- summary[paste0("beta.c[",1:C,"]"), "50%"]
# # alphac <- summary[paste0("a.c[",1:C,"]"), "50%"]
# temp <- readRDS(file.path("/home/greggu/git/limiting_spacing_breakdown/output/vary_all", "mod.rds"))
# mcmc.array <- temp$BUGSoutput$sims.array
# summary <- temp$BUGSoutput$summary
# betac_vary <- summary[paste0("beta.c[",1:C,"]"), "50%"]
# # alphac <- summary[paste0("a.c[",1:C,"]"), "50%"]
# temp <- readRDS(file.path("/home/greggu/git/limiting_spacing_breakdown/output/vary_all_cor", "mod.rds"))
# mcmc.array <- temp$BUGSoutput$sims.array
# summary <- temp$BUGSoutput$summary
# betac_vary_cor <- summary[paste0("beta.c[",1:C,"]"), "50%"]
# yrep.i <- summary[paste0("yrepluup.i[",1:n,"]"), "50%"]
# temp2 <- data.frame(yrep, getnc.i, getnt.i)
# colnames(temp2) <- c("yrep","Country", "year")
# # alphac <- summary[paste0("a.c[",1:C,"]"), "50%"]
# 
# 
# temp <- data.frame(betac, betac_vary, betac_vary_cor, name.c)
# colnames(temp) <- c("betac", "betac_vary", "betac_vary_cor","Country")
# # library(lme4)
# # fit <- lmer(logit_PDL ~ (1+demand | Country/Region), data=plotdf)
# # #fit2 <- lmer(logit_PDL ~ (1 | Country/Region) + (demand | Country), data=plotdf)
# # temp <- data.frame(coef(fit)$Country)
# # temp <- data.table::setDT(temp,keep.rownames=TRUE)
# # temp$betac <- betac
# # temp$betac <- betac
# # temp2 <- data.frame(coef(fit2)$Country)
# # temp2 <- data.table::setDT(temp2,keep.rownames=TRUE)
# # colnames(temp2)<-c("Country","intercept","demand_slope")
# # colnames(temp)<-c("Country","beta_vary_all","intercept","beta_fixed")
# # temp$demand_slope_fixB <- temp2$demand_slope
# plotdf <- merge(plotdf, temp, by = "Country")
# plotdf <- merge(plotdf, temp2, by = c("Country","year"))
# # r2.corr.mer <- function(m) {
# #   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
# #   summary(lmfit)$r.squared
# # }
# # r2.corr.mer(fit)
# # r2.corr.mer(fit2)
# 
# #lets get betas from just lm
# .ml <- list()
# for (c in 1:length(unique(getnc.i))) {
#   temp <- data.frame(subset(plotdf, Country == unique(getnc.i)[c]))
#   #if (nrow(temp) > 2 ) {next}
#   fit <- lm(data=temp, logit_PDL~demand)
#   .ml[[c]]<-fit$coefficients[2]
# }
# 
# temp<-data.frame(unlist(.ml))
# temp[is.na(temp)] <- 0
# temp$Country <- unique(getnc.i)
# colnames(temp) <- c("beta_fix_all","Country")
# plotdf <- merge(plotdf, temp, by="Country")

