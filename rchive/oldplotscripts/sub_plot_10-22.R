
#temp <- plotdf[order(plotdf$demand),]
target <- "fig"

temp <- plotdf

p <- ggplot(data=temp,aes(demand, PDL, color=Country)) +
  geom_point(alpha=1,size=.6) +
  geom_line(size=.6,alpha=.2) +
  geom_line(aes(demand, yrep, color=Country), size=1, lty=2, alpha=.6) +
  theme(legend.position = "none")+
  facet_wrap(~Region)
#line to print
target_stub <- file.path(target, "prediction_VS_dots")
png(paste0(target_stub, ".png"), height = 700, width = 1000)
print(p); dev.off()

p <- ggplot(data=temp,aes(demand, PDL, color=Country)) +
  geom_point(alpha=.25) +
  geom_smooth(method=lm, size=.75,alpha=1, se=FALSE) +
  geom_line(aes(demand, yrep, color=Country), size=1, lty=2, alpha=.5) +
  theme(legend.position = "none") +
  facet_wrap(~Region)
#line to print
target_stub <- file.path(target, "prediction_VS_lm")
png(paste0(target_stub, ".png"), height = 700, width = 1000)
print(p); dev.off()




temp <- plotdf %>% subset(year < 2007)
target <- "fig/sub_2007"
if (!dir.exists('fig/sub_2007'))
  dir.create('fig/sub_2007')


p <- ggplot(data=temp,aes(demand, PDL, color=Country)) +
  geom_point(alpha=1,size=.6) +
  geom_line(size=.6,alpha=.2) +
  geom_line(aes(demand, yrep, color=Country), size=1, lty=2, alpha=.6) +
  theme(legend.position = "none")+
  facet_wrap(~Region)
#line to print
target_stub <- file.path(target, "prediction_VS_dots")
png(paste0(target_stub, ".png"), height = 700, width = 1000)
print(p); dev.off()

p <- ggplot(data=temp,aes(demand, PDL, color=Country)) +
  geom_point(alpha=.25) +
  geom_smooth(method=lm, size=.75,alpha=1, se=FALSE) +
  geom_line(aes(demand, yrep, color=Country), size=1, lty=2, alpha=.5) +
  theme(legend.position = "none") +
  facet_wrap(~Region)
#line to print
target_stub <- file.path(target, "prediction_VS_lm")
png(paste0(target_stub, ".png"), height = 700, width = 1000)
print(p); dev.off()
