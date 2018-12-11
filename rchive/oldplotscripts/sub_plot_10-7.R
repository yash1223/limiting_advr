

#subset betas for abs min change in demand #
#subset past plots by year 2007ish
#re do lag plots with our function
#re do lag with subsets?

library(ggplot2)
if (!dir.exists('fig'))
  dir.create('fig')
target <- "fig"

temp <- subset(plotdf, year > 2007)
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_lm(data = temp, x=demand, y=PDL, group=Region, country=Country)
target_stub <- file.path(target, "xy_2007subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_c(data = temp, x=demand, y=PDL, group=Region, country=Country)
target_stub <- file.path(target, "xy_2007subsetc")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_lag, y=logit_PDL_lag, group=Region, country=Country)
target_stub <- file.path(target, "lag_2007subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_lag_adj, y=logit_PDL_lag_adj, group=Region, country=Country)
target_stub <- file.path(target, "lag_adj_2007subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_avg, y=logit_PDL_lag, group=Region, country=Country)
target_stub <- file.path(target, "lag_pdl_averagedemand_2007subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()

temp <- plotdf %>% group_by(Country) %>% as.data.frame()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_avg, y=demand_slope, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_avg")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_change, y=demand_slope, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_changedemand")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
temp <- subset(temp, demand_change > .05)
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_avg, y=demand_slope, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_avg_subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_change, y=demand_slope, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_changedemand_subset")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()




#10-8 plots
temp <- plotdf %>% 
  subset(year < 2007) %>%
  subset(demand_change > .05) %>%
  subset(demand_slope_lm.x !=0) %>%
  group_by(Country) %>% as.data.frame()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_avg, y=demand_slope_lm.x, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_avg_lm1_2007b")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()

temp <- plotdf %>% 
  subset(year < 2007)
  subset(demand_slope_lm < 4) %>% 
  subset(demand_slope_lm >-4) %>% 
  subset(demand_slope_lm !=0) %>%
  group_by(Country) %>% as.data.frame()
pll = plot_list(names=unique(as.character(temp$Region))) %>% 
  plot_general(data = temp, x=demand_avg, y=demand_slope_lm.x, group=Region, country=Country)
target_stub <- file.path(target, "Beta_v_avg_lm2")
pdf(paste0(target_stub, ".pdf"), height = 4, width = 4)
for (pl in pll) print(pl); dev.off()
