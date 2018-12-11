source("sub_EDA_preproc.R")

temp <- plotdf
target <- "fig"

#10-16 plots
temp <- temp %>% 
  subset(beta_fix_all != 0) %>%
  subset(beta_fix_all < 4) %>%
  subset(beta_fix_all > -4) %>%
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=beta_fix_all, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_fixall")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()

#vary intercept fix slope
temp <- temp %>% 
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_fixB")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()


#vary intercept fix slope
temp <- temp %>% 
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac_vary, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_varyB")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()

#vary intercept fix slope
temp <- temp %>% 
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac_vary_cor, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_varyB_cor")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()




temp <- plotdf %>% subset(year < 2007)
target <- "fig/sub_2007"
if (!dir.exists('fig/sub_2007'))
  dir.create('fig/sub_2007')

#10-16 plots
temp <- temp %>% 
  subset(beta_fix_all != 0) %>%
  subset(beta_fix_all < 4) %>%
  subset(beta_fix_all > -4) %>%
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=beta_fix_all, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_fixall")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()

#vary intercept fix slope
temp <- temp %>% 
  #  subset(year > 2007) %>%
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_fixB")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()


#vary intercept fix slope
temp <- temp %>% 
  #  subset(year > 2007) %>%
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac_vary, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_varyB")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()


#vary intercept fix slope
temp <- temp %>% 
  group_by(Country) %>% as.data.frame()
pll <- plot_general(data = temp, x=demand_avg, y=betac_vary_cor, country=Country, region=Region)
target_stub <- file.path(target, "Beta_v_avg_varyB_cor")
png(paste0(target_stub, ".png"), height = 480, width = 1000)
for(out in pll) print(out); dev.off()








