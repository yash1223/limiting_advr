

path <- getwd()
path <- file.path(path,"fig/oct")
# Lets look at only max year (possibly most accurate?)
pdf(file.path(path,"maxyear.pdf"),width = 12, height = 7)
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
ggplot(data=temp, aes(x=demand, y=luup, color=Region)) + geom_point() + geom_smooth() + ggtitle("maxyear")
ggplot(data=temp, aes(x=demand, y=luup, color=Region)) + geom_point() + geom_smooth(method = "loess", size =1, lty = 2, colour="black")
dev.off()

# Lets get the difference between the max year and min year for each country
# This will give us the change in variables over respective records
pdf(file.path(path,"maxmin.pdf"),width = 12, height = 7)
temp <- plotdf %>% group_by(Country) %>% filter(year == max(year))
temp2 <- plotdf %>% group_by(Country) %>% filter(year == min(year))
temp$pdl_change <- temp$luup - temp2$luup
temp$demand_change <- temp$demand - temp2$demand
ggplot(data=temp, aes(x=demand_change, y=pdl_change, color=Region)) + geom_point() + geom_smooth() + ggtitle("max(year) - min(year)")
ggplot(data=temp, aes(x=demand_change, y=pdl_change, color=Region)) + geom_point() + geom_smooth(method = "loess", size =1, lty = 2, colour="black")
dev.off()

# Lets get the change between subsequent observations for each country
pdf(file.path(path,"Diff.pdf"),width = 12, height = 7)
temp <- plotdf %>%
  group_by(Country) %>%
  mutate(Diff_pdl = luup - lag(luup))
temp <- temp %>%
  group_by(Country) %>%
  mutate(Diff_demand = demand - lag(demand))
temp$Diff_pdl[is.na(temp$Diff_pdl)] <- 0
temp$Diff_demand[is.na(temp$Diff_demand)] <- 0
ggplot(data=temp, aes(x=Diff_demand, y=Diff_pdl, color=Region)) + geom_point() + geom_smooth(method = "loess", size =1, lty = 2, colour="black") + ggtitle("lag plot")
dev.off()

# adjusting for year and logit
pdf(file.path(path,"Diff_logit_year.pdf"),width = 12, height = 7)
temp <- plotdf %>%
  group_by(Country) %>%
  mutate(Diff_pdl = (logit_luup - lag(logit_luup))/(year-lag(year)))
temp <- temp %>%
  group_by(Country) %>%
  mutate(Diff_demand = (demand - lag(demand))/(year-lag(year)))
temp$Diff_pdl[is.na(temp$Diff_pdl)] <- 0
temp$Diff_demand[is.na(temp$Diff_demand)] <- 0
ggplot(data=temp, aes(x=Diff_demand, y=Diff_pdl, color=Region)) + geom_point() + geom_smooth(method = "loess", size =1, lty = 2, colour="black") + ggtitle("lag plot, adjusted for year, on logit scale")
dev.off()

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
