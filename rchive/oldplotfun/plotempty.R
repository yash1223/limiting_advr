PlotEmpty <- function(main  = NULL, ylab = NULL, 
                      maxy = 1, miny = 0, addrightaxis = FALSE, 
                      year.range = c(2000,2017)){
  plot(1, type = "n", 
       main = main, #paste(name.c[c]," (", group.c[c], ")", sep = ""), 
       xlab = "Year", 
       ylab = ylab,
       xlim = year.range,
       ylim = c(miny, ifelse(maxy==-Inf, 1,maxy)), yaxt = "n")
  multiplier <- 1 #ifelse(outcome=="mmr", 10^5,1)
  seqplot <- GetNiceSequence(xmin =miny*multiplier, xmax = maxy*multiplier)
  axis(2,at = seqplot/multiplier, label = seqplot, las = 2)
  if (addrightaxis)   axis(4,at = seqplot/multiplier, label = seqplot, las = 2)
}


#-----------------
PlotCIbands <- function(CIs.qt, seq.years, col = "#0000FF30"){
  ## 95% CI plolygon plot over time ##
  # assuming q is 3
  CI.low.t <- CIs.qt[1,]
  CI.up.t  <- CIs.qt[3,]
  x.full <- c(seq.years, rev(seq.years),seq.years[1])
  y.full <- c(CI.low.t, rev(CI.up.t), CI.low.t[1])
  
  nonNA <- !is.na(y.full) & !is.na(x.full) #only plot points with non-missing (x,y)
  x.plot <- x.full[nonNA]
  y.plot <- y.full[nonNA]
  polygon(x.plot,y.plot,col=col, border = NA)
}

#--------------------------------------------------------
makeTransparent<-function(someColor, alpha=100){
  #note: always pass alpha on the 0-255 scale
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

#--------------------------------------------------------
PlotEmptyForLegend <- function(){
  plot(1, type = "n", bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
}

#--------------------------------------------------------------------
#getsequence nice was updated but not fully tested
GetNiceSequence <- function(
  xmin =0, 
  xmax){
  pretty(x = c(xmin, xmax))
  #   # assume that xmin is a nice number already...
  #   # get outcome between 0 and 1 times multiplier...
  #   # should be an easier way!!
  #   l <-  length(strsplit(as.character(round(xmax-xmin)), split = NULL)[[1]])  
  #   #length(strsplit(as.character(round(0)), split = NULL)[[1]])  
  #   multiplier <- 10^ifelse(l==1,0, l)
  #   xmin.new <- xmin/multiplier
  #   xmax.new <- xmax/multiplier
  #   if ((xmax.new - xmin.new) <= 0.001){
  #     seq.nice <- multiplier*seq(xmin.new, 0.002+xmax.new, by = 0.0001)
  #     return(seq.nice)
  #   }
  #   if ((xmax.new - xmin.new) <= 0.05){
  #     seq.nice <- multiplier*seq(xmin.new, 0.02+xmax.new, by = 0.01)
  #     return(seq.nice)
  #   }
  #   if ((xmax.new - xmin.new) <= 0.1){
  #     seq.nice <- multiplier*seq(xmin.new, 0.04+xmax.new, by = 0.02)
  #     return(seq.nice)
  #   }
  #   if ((xmax.new - xmin.new) <= 0.4){
  #     seq.nice <-multiplier*seq(xmin.new, 0.1+xmax.new, by = 0.05)
  #     return(seq.nice)
  #   }
  #   if ((xmax.new - xmin.new) <= 1){
  #     seq.nice <- multiplier*seq(xmin.new, min(1, 0.1+xmax.new), by = 0.1)
  #     return(seq.nice)
  #   }
}

#--------------------------------------------------------------------
GetNiceSequenceLog <- function(
  # better for logscale
  xmin =0, 
  xmax){
  # assume that xmin is a nice number already...
  # get outcome between 0 and 1 times multiplier...
  # should be an easier way!!
  l <-  length(strsplit(as.character(round(xmax)), split = NULL)[[1]])  
  multiplier <- 10^ifelse(l==1,0, l)
  xmin.new <- xmin/multiplier
  xmax.new <- xmax/multiplier
  if ((xmax.new - xmin.new) <= 0.05){
    seq.nice <- multiplier*seq(xmin.new, 0.02+xmax.new, by = 0.005)
    return(seq.nice)
  }
  if ((xmax.new - xmin.new) <= 0.1){
    seq.nice <- multiplier*seq(xmin.new, 0.04+xmax.new, by = 0.005)
    return(seq.nice)
  }
  if ((xmax.new - xmin.new) <= 0.4){
    seq.nice <-multiplier*seq(xmin.new, 0.1+xmax.new, by = 0.005)
    return(seq.nice)
  }
  if ((xmax.new - xmin.new) <= 1){
    seq.nice <- multiplier*seq(xmin.new, min(1, 0.1+xmax.new), by = 0.05)
    return(seq.nice)
  }
}

#GetNiceSequence(xmin =0, xmax=1)
#GetNiceSequence(xmin =0, xmax=56)
#GetNiceSequence(xmin =0, xmax=981)


PlotScatter <- function(estimate1, estimate2,iso.label=meta$iso.c,ylab=NULL,xlab=NULL,main=NULL,col.group=NULL,col=1:8,cutoff=0.10,plot.range=NULL,pch=20,pch.with.textlabel=FALSE){
  y<-estimate1[!is.na(estimate1)&!is.na(estimate2)]
  x<-estimate2[!is.na(estimate1)&!is.na(estimate2)]
  iso.label <- iso.label[!is.na(estimate1)&!is.na(estimate2)]
  
  sel <- abs(abs(y)-abs(x))/abs(x)>cutoff
  if(is.null(plot.range)){
    plot.range<-c(min(y,x,0),max(y,x)*1.05)
  }
  
  if(!is.null(col.group)){
    col.group <- col.group[!is.na(estimate1)&!is.na(estimate2)]
    groups <- as.factor(sort(unique(col.group)))
    col.use <-col[match(col.group,groups)]
  }else{
    col.use<-rep(1,length(y))
  }
  
  plot(y~x,main=main,type="n",ylim=plot.range,xlim=plot.range,ylab=ylab,xlab=xlab)
  abline(0,1)
  points(y[!sel]~x[!sel],pch=pch,col=col.use[!sel])
  if(sum(sel)>0){
    if(pch.with.textlabel){points(y[sel]~x[sel],pch=pch,col=col.use[sel])}
    text(x=x[sel],y=y[sel],labels=iso.label[sel],cex=1,col=col.use[sel],pos=4)
  }
  if(!is.null(col.group)){
    legend("bottomright",legend=groups,lty=-1,pch=-1,text.col=col[groups],bty="n",xjust=0,cex=2)
  }
  
}


addEstimates <- function( CIs.qt = NULL, ## can read in either CIs.qt 
                          estimates = NULL, ## or just a vector of estimates 
                          seq.years = 1985:2015, 
                          lty = 1,
                          col = 1,
                          lwd = 3){
  if(is.null(CIs.qt) & is.null(estimates)){print("No estimates to plot!");break}
  
  if(!is.null(CIs.qt)){
    y <- CIs.qt[2,]
  }else{
    y <- estimates 
  }
  x <- seq.years
  nonNA <- !is.na(y) & !is.na(x)
  y.plot <- y[nonNA]
  x.plot <- x[nonNA]
  lines(y.plot ~ x.plot,lty=lty,col=col,lwd=lwd)
}
## rename so it is more consistent (add suggest that the function need to be called after plot has been called )
addCIbands <- function(CIs.qt, seq.years, col = "#0000FF30"){
  ## 95% CI plolygon plot over time ##
  # assuming q is 3
  CI.low.t <- CIs.qt[1,]
  CI.up.t  <- CIs.qt[3,]
  x.full <- c(seq.years, rev(seq.years),seq.years[1])
  y.full <- c(CI.low.t, rev(CI.up.t), CI.low.t[1])
  
  nonNA <- !is.na(y.full) & !is.na(x.full) #only plot points with non-missing (x,y)
  x.plot <- x.full[nonNA]
  y.plot <- y.full[nonNA]
  polygon(x.plot,y.plot,col=col, border = NA)
}

# LA2Sq: think you should set data =NULL and explain what's expected instead
addData <- function (data=data.frame(year,start,end,type,pointestimate),
                     type=c("vr","inq","dhs","census","misc","excl-0pm","excl-vr","excl-vr-overlap","excl-misc"),
                     type.label=c("VR","Spec. studies","DHS","Census","Misc. studies",
                                  "Reported VR before recalc.\n(related to 0 obs. mat. death)","Excluded VR data \n(data quality issues)","Excluded VR data \n(overlap with specialized study)","Excluded misc. studies"),
                     pch=c(rep(23,5),15,16,17,18), #define pch according to type 
                     col=c(colorsr[6],"purple",4,6,2,colorsr[8],colorsr[8],colorsr[8],colorsr[8]),
                     pch.bg=NA, cex = 1.5,
                     plot.time.segment=TRUE,
                     print.legend.label=TRUE,
                     plot.excluded.data=TRUE
                     
){
  
  
  if(plot.excluded.data==FALSE){
    remove <- which(is.element(data$type,c("excl-0pm","excl-vr","excl-vr-overlap","excl-misc")))
    if(length(remove)>0){
      data <- data[-remove,]
    }
    
  }
  ## by default, fill by colors defined (for unadjusted data - fill with white)
  if(is.na(pch.bg)){pch.bg <- col}
  if(length(pch.bg)==1){pch.bg<-rep(pch.bg,length(col))}
  ##outputs of the function 
  ## 1. add points onto graph 
  ## 2. legend details 
  # LA2Sq: why not use data$type instead?
  # LA201802 HACK
  type.plotted <- is.element(type,data[,4])
  #type.plotted <- 1
  ### do not differentiate between MMR and PM - just read in pointestimate 
  index<-match(data[,4],type)
  pch.use <- pch[index]
  col.use <- col[index]
  pch.bg.use <- pch.bg[index]
  # LA2Sq: why not use data$start etc instead? perhaps tricky for est, perhaps have that as a separate argument obs = ...
  if(plot.time.segment){
    segments(data[,2], data[,5], data[,3], data[,5], col = col.use,  lwd = 3)
  }
  points(data[,5]~data[,1],pch=pch.use,col=col.use,bg=pch.bg.use,cex=cex)
  
  if(print.legend.label){
    return(list(label=type.label[type.plotted],
                pch=pch[type.plotted],
                col=col[type.plotted],
                pch.bg=pch.bg[type.plotted],
                lty=rep(NA,sum(type.plotted)),
                lwd=rep(NA,sum(type.plotted))
    ))
  }
  
  
}
addIntervalForData <- function (data = data.frame(year,type,lower.percentile.to.print,upper.percentile.to.print),
                                type=c("vr","inq","dhs","census","misc","excl-0pm","excl-vr","excl-vr-overlap","excl-misc"),
                                col=c(colorsr[6],"purple",4,6,2,colorsr[8],colorsr[8],colorsr[8],colorsr[8]),lwd=5,lty=1
){
  color.use<-tapply(col[match(data[,2],type)],1:length(data[,2]),function(x) makeTransparent(x,50))
  segments(data[,1],data[,3],data[,1],data[,4],col=color.use,lwd=lwd,lty=lty)
}

addJoinRawAdj <- function(data = data.frame(year,type,raw,adj),
                          type=c("vr","inq","dhs","census","misc","excl-0pm","excl-vr","excl-vr-overlap","excl-misc"),
                          col=c(colorsr[6],"purple",4,6,2,colorsr[8],colorsr[8],colorsr[8],colorsr[8]),lwd=1,lty=3, cex = 1
){
  color.use<-col[match(data[,2],type)]
  segments(data[,1],data[,3],data[,1],data[,4],col=color.use,lwd=lwd,lty=lty, cex = cex)
}