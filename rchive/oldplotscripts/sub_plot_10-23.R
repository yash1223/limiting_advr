#working with plot_jags
target <- "fig"

plot_jags <- readRDS("data/plotjags1.rds")
plot_jags$residual <- plot_jags$jagsdata.luup.i - plot_jags$X50.
ggplot(data=plot_jags) +
  geom_point(aes(x=demand.i, y=jagsdata.luup.i, alpha= .6)) +
  geom_point(aes(x=demand.i, y=X50., color=2, alpha=.6))
target_stub <- file.path(target, "1")
png(paste0(target_stub, ".png"), height = 480, width = 1000)

ggplot(data=plot_jags) +
  geom_point(aes(x=getnt.i, y=residual)) +
  geom_smooth(method="loess",aes(x=getnt.i, y=residual))
ggplot(data=plot_jags) +
  geom_point(aes(x=demand.i, y=residual)) +
  geom_smooth(method="loess",aes(x=demand.i, y=residual))

plot_cor <- readRDS("data/plotjagscor1.rds")
plot_cor$country <- name.c
plot_cor$region <- region.c
ggplot(data=plot_cor, aes(x=a.c, y=beta.c, color=region)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~region)
ggplot(data=plot_cor, aes(x=a.c, y=beta.c, color=region)) +
  geom_point() +
  facet_wrap(~region)



plot_jags <- readRDS("data/plotjags2.rds")
plot_jags$residual <- plot_jags$luup.i - plot_jags$X50.
ggplot(data=plot_jags) +
  geom_point(aes(x=demand.i, y=luup.i, alpha= .6)) +
  geom_point(aes(x=demand.i, y=X50., color=2, alpha=.6))
target_stub <- file.path(target, "1")
png(paste0(target_stub, ".png"), height = 480, width = 1000)

ggplot(data=plot_jags) +
  geom_point(aes(x=getnt.i, y=residual)) +
  geom_smooth(method="loess",aes(x=getnt.i, y=residual))


plot_jags <- readRDS("data/plotjags3.rds")
plot_jags$residual <- plot_jags$luup.i - plot_jags$X50.
ggplot(data=plot_jags) +
  geom_point(aes(x=demand.i, y=luup.i, alpha= .6)) +
  geom_point(aes(x=demand.i, y=X50., color=2, alpha=.6))
target_stub <- file.path(target, "1")
png(paste0(target_stub, ".png"), height = 480, width = 1000)

ggplot(data=plot_jags) +
  geom_point(aes(x=getnt.i, y=residual)) +
  geom_smooth(method="loess",aes(x=getnt.i, y=residual))
