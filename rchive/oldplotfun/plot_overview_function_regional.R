


plot_country_branches_regional = function(plotname, data, country_column, region_column, xvar, yvar) {
  pdf(plotname, width = 12, height = 7)
  #first prints a global relationship
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column))+
          geom_point(aes())+
          geom_smooth(method = "loess", size =1, lty = 2, colour="black")+
          labs(x=xvar, y=yvar, title="Global Relationship") +
          scale_y_continuous(limits=c(0,1)) + 
          coord_cartesian(ylim = c(0, 1)))
          #theme(legend.position="none"))
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column, group=country_column))+
          geom_point(aes())+
          geom_line() +
          labs(x=xvar, y=yvar, title="connect the dots") +
          scale_y_continuous(limits=c(0,1)) + 
          coord_cartesian(ylim = c(0, 1))) 
          #theme(legend.position="none"))
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column, group=country_column))+
          geom_point(aes())+
          geom_smooth(method="lm", se=FALSE) +
          labs(x=xvar, y=yvar, title="lm per country") +
          scale_y_continuous(limits=c(0,1)) + 
          coord_cartesian(ylim = c(0, 1))) 
          #theme(legend.position="none"))  
dev.off()
}
  

plot_country_branches_regional_edit = function(plotname, data, country_column, region_column, xvar, yvar) {
  pdf(plotname, width = 12, height = 7)
  #first prints a global relationship
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column))+
          geom_point(aes())+
          geom_smooth(method = "loess", size =1, lty = 2, colour="black")+
          labs(x=xvar, y=yvar, title="Global Relationship"))
  #theme(legend.position="none"))
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column, group=country_column))+
          geom_point(aes())+
          geom_line() +
          labs(x=xvar, y=yvar, title="connect the dots"))
  #theme(legend.position="none"))
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column, group=country_column))+
          geom_point(aes())+
          geom_smooth(method="lm", se=FALSE) +
          labs(x=xvar, y=yvar, title="lm per country"))
  #theme(legend.position="none"))  
  dev.off()
}