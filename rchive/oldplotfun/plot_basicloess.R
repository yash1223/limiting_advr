

plot_regional_loess = function(plotname, data, country_column, number_of_regions, region_names, region_vector, xvar, yvar, xlab, ylab, region_column) {
  pdf(plotname, width = 12, height = 7)
  #first prints a global relationship
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column))+
          geom_point(aes())+
          geom_smooth(method = "loess", size =1, lty = 2, colour="black")+
          # labs(x=xvar, y=yvar, title="Global Relationship") +
          ggtitle("Global Relationship") +
          xlab(as.character(xlab)) + ylab(as.character(ylab))+
          theme(title = element_text(size=22, face="bold"),axis.title = element_text(size=20)))

  for (r in 1:number_of_regions){
    temp=region_vector==as.character(region_names[r])
    temp2=data[temp,]
    print(ggplot(data=temp2, aes_string(x=xvar, y=yvar, color=country_column))+
            geom_point(aes())+
            geom_smooth(method = "loess", size =1, lty = 2, colour="black")+
            # labs(x=xvar, y=yvar, title="Global Relationship") +
            ggtitle(paste0(region_names[r])) +
            xlab(as.character(xlab)) + ylab(as.character(ylab))+
            theme(legend.position="none", title = element_text(size=22, face="bold"),axis.title = element_text(size=20)))
    # labs(x=xvar, y=yvar, title=as.character(region_names[r])))
  }
  
  
  dev.off()
}

plot_one_loess = function(plotname, data, country_column, number_of_regions, region_names, region_vector, xvar, yvar, xlab, ylab, region_column) {
  pdf(plotname, width = 12, height = 7)
  #first prints a global relationship
  temp2=data[oneplot==1,]
  
  print(ggplot(data=temp2, aes_string(x=xvar, y=yvar, color=country_column))+
          geom_point(aes())+
          geom_smooth(method = "loess", size =1, lty = 2, colour="black")+
          # labs(x=xvar, y=yvar, title="Global Relationship") +
          ggtitle("asia/ssafrica") +
          xlab(as.character(xlab)) + ylab(as.character(ylab))+
          theme(legend.position="none", title = element_text(size=22, face="bold"),axis.title = element_text(size=20)))
  
  dev.off()
}
