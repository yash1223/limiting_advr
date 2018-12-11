plot_customlm = function(plotname, data, country_column, region_column, xvar, yvar, yhat, region_names, number_of_regions, region_vector) {
  pdf(plotname, width = 12, height = 7)
  #first prints a global relationship
  print(ggplot(data=data, aes_string(x=xvar, y=yvar, color=region_column))+
          geom_point(aes())+
          labs(x=xvar, y=yvar, title="Global Relationship"))
  #theme(legend.position="none"))
  for (r in 1:number_of_regions){
    temp=region_vector==as.character(region_names[r])
    temp2=data[temp,]
    print(ggplot(data=temp2, aes_string(x=xvar, y=yvar, color = country_column, group=country_column)) + 
            geom_point(aes()) +
            geom_line(aes_string(x=xvar, y=yhat, color=country_column))+
            labs(x=xvar, y=yvar, title=as.character(region_names[r])))
  }
  dev.off()
}