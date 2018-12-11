
getall <- function(r,p,q){
  x1 <- r*p*q/(1-q + r*q)
  x2 <- p-x1
  x3 <- q - x1 
  x4 <- 1- x1-x2-x3
  return(cbind(x1,x2,x3,x4))
} 

fromxtopqr <- function(x){
  p <- x[1]+x[2]
  q <- x[1]+x[3]
  r <- x[1]/(x[1]+x[2]) / (x[3]/(x[3]+x[4]))
  return(c(p,q,r))
}

GetRatio <- function(luup, lup, cpr, demand){
  alllimiters <- demand*luup 
  allspacers <- demand - alllimiters
  unmet <- demand - cpr
  ul <- lup*unmet
  us <- unmet - ul
  ratio <- ((1-ul/alllimiters)/(1 - us/allspacers))
  return(ratio)
}

# S <- 1000
# p <- 0.5
# q <- 0.6
# x1.s <- runif(S, max(0, p+q-1), min(p,q))
# x2.s <- p - x1.s
# x3.s <- q - x1.s
# x4.s <- 1 - (x1.s + x2.s + x3.s)
# res.fs <- apply(cbind(x1.s, x2.s, x3.s, x4.s),1, fromxtopqr)
# res.fs[3,]
# 
# GetRatio()
GetLup <- function(ratio, luup, cpr, demand){
  alllimiters <- demand*luup 
  allspacers <- demand - alllimiters
  clovercs <-ratio*alllimiters/allspacers
  clpluscs <- cpr
  cs <- clpluscs/(clovercs+1)
  cl <- cpr - cs
  ul <- alllimiters - cl
  
  (cl/alllimiters)/(cs/allspacers)
  
  lup <- ul/(demand - cpr)
  return(lup)
}
# GetLup(ratio =3, luup = 0, cpr = 0.9, demand = 1)
# 0.5+0.9-1
# 
# GetLup(ratio =1.05, luup = 0.7, cpr = 0.807, demand = 0.876)
# GetLup(ratio =1.3, luup = 0.7, cpr = 0.807, demand = 0.876)



# r <- 1.05
# q <-  0.807/0.876 
# p <- 0.7
getx1 <- function(r) r*p*q/(1-q + r*q)
frompqrtox <- function(p,q,r){
  x1 <- getx1(r)
  #x1 <- getx1(r =1.3)
  x2 <- p - x1
  x3 <- q - x1
  x4 <- 1-x1-x2-x3
  return(c(x1,x2,x3,x4))
}
fromxtopqr <- function(x){
  p <- x[1]+x[2]
  q <- x[1]+x[3]
  r <- x[1]/(x[1]+x[2]) / (x[3]/(x[3]+x[4]))
  return(c(p,q,r))
}
# select <- which.max(getc.i == which(name.c=="Colombia"))
# fromxtopqr(cbind(cl, ul, cs, us)[select,]/sum(cbind(cl, ul, cs, us)[select,]))
# cbind(luup, cl+cs, ratio)[select,]
# (cl+cs)[select]/sum(cbind(cl, ul, cs, us)[select,])
# 
# r=1.3  
# fromxtopqr(frompqrtox(p,q,r)); p;q;r
# 
# 
# lup <- x2/(x2+x4); lup
# 
# ((cl+cs)/totdemand)[getc.i == which(name.c=="Colombia")]
# q
# luup[getc.i == which(name.c=="Colombia")]
# (ul/unmet)[getc.i == which(name.c=="Colombia")]
# lup[getc.i == which(name.c=="Colombia")]
# ratio.i[getc.i == which(name.c=="Colombia")]
# 
# curve(getx1(x), xlim = c(0,100))
# abline(h=p+q-1)
# abline(h=min(p,q))
# 
# x1 <- getx1(3)
# x2 <- p-x1; x2
# x3 <- q - x1; x3
# 1-x1-x2-x3
# 
# p <- runif(S)
# q <- runif(S)
# mean( (p+q-1) <= min(p,q))

getall <- function(r,p,q){
  x1 <- r*p*q/(1-q + r*q)
  x2 <- p-x1
  x3 <- q - x1 
  x4 <- 1- x1-x2-x3
  return(cbind(x1,x2,x3,x4))
} 
# set.seed(12)
# p.s <- runif(S)
# q.s <- runif(S)
# r.s <- seq(0,S-1)
# res <- getall(r.s, p.s, q.s)
# apply(res,1,sum)
# sum(res<0)
# # all in x4
# sum(apply(res<0,1, sum) & !(res[,4] > 0))
# 
# res[apply(res<0,1,sum)>0,][1:5,]
# cbind(r.s, p.s, q.s)[apply(res<0,1,sum)>0,][1:5,]
# sum(res>1)
# 
# rationew <- function(a,b,d) a+b*d-(a+b)*d^2
# curve(rationew(-1,-2,x), xlim = c(0,1))



# obtain combis of cells that add to 1
#library(MCMCpack)
#S <- 1000
#samp.sc <- rdirichlet(S, alpha = rep(1/4,4))
#apply(samp.sc,1,sum)
# S <- 1000
# p <- 0.5
# q <- 0.6
# x1.s <- runif(S, max(0, p+q-1), min(p,q))
# x2.s <- p - x1.s
# x3.s <- q - x1.s
# summary(1 - (x1.s + x2.s + x3.s))
# 
# p <- 0.5
# q <- 0.6
# p+q-1
# r <- 1
# x1 <- r*p*q/(1-q + r*q); x1
# p <- 0.5
# q <- 0.9
# 2 
# p <- 0#0.9426217 
# q <- 0.808246720
# 
# GetLup(ratio =1.05, luup = 0.7, cpr = 0.807, demand = 0.876)
# GetLup(ratio =1.3, luup = 0.7, cpr = 0.807, demand = 0.876)
