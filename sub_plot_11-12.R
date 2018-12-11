
plot_jags$residual <- plot_jags$jagsdata.luup.i - plot_jags$X50.
ggplot(data=plot_jags) +
  geom_point(aes(x=getnt.i, y=residual)) +
  geom_smooth(method="loess",aes(x=getnt.i, y=residual)) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title = element_text(size = 13, face="bold"))


ggplot(data=plot_jags) +
  geom_point(aes(x=demand.i, y=residual)) +
  geom_smooth(method="loess",aes(x=demand.i, y=residual))
