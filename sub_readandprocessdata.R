rm(jagsdata)
df = read.csv(rawdata_dhs.dir)
df$surv <- rep("dhs",nrow(df))
df2 = read.csv(rawdata_mics.dir)
df2$surv <- rep("mics", nrow(df2))
fp20 = read.csv(fp20.dir)
# note: sudan and iraq have 0 cp_limiting, while in another survey, sudan had fair % so leave out
excludeddf2 <- df2$G1.1_Using_Limit==0
df[1] <- NULL # removes 
df = rbind(df, df2[!excludeddf2,])
#df$ISO.code    
#df$RefDate
df$mid.year <- floor(df$RefDate)
df <- df %>% filter(mid.year <= endyear) 
df <- df %>% filter(mid.year >= startyear)


# Working with Country-area-classification file
# Import data
area <- read.csv(countryinfo.dir)
names(area)[names(area)=="Country.or.area"] <- "country"

# Logistic Vectors data length to identigy subregions we are merging
# _d denotes vector of length(data file)
comp_d<-area$Region=="Western Asia"|area$Region=="Northern Africa"
# western asia or north africa are merged to a large region called W Asia and N Africa
# create a new region variable
area$Major.area_d<-as.character(area$Major.area)
# Finally, add in the new region
area$Major.area_d[comp_d]<-'W Asia and N Africa'
area$Major.area_d<-factor(area$Major.area_d)
colnames(area) <- FixIsoName(varnames = colnames(area))
#colnames(area)[2] <- "ISO.code" # this is neccesary C was capitalized
# exclude European coutnries (not in FP2020)
area <- area[which(area$Major.area_d != "Europe"),]

# usually we'd use the country order given in this file (as its the unique set of countries)
# but it includes more than we need, so we'll go for the isos from demand file instead


# edit: remove all isos without data!
select <- which(is.element(area$ISO.code, df$ISO.code))
area <- area[select, ]

#cppred <- merge(cppred, area, by="ISO.code")
#dim(cppred)
cppred <- area
if (SSA== TRUE){
  cppred <- cppred %>% filter(Major.area_d == "Africa")
}
if (NOTSSA == TRUE){
  cppred <- cppred %>% filter(Major.area_d != "Africa")
}
  
iso.c <- cppred$ISO.code 
C <- length(iso.c)
name.c <- cppred$country
region.c <- cppred$Major.area_d
regnames <- unique(region.c)
R <- length(regnames)
# I've had problems with code below before so i just hardcode it here
#getr.c <- as.numeric(factor(region.c, levels = regnames))
#getc.i <- as.numeric(factor(df$ISO.code, levels = iso.c))
getr.c <- Getc.i(iso.i = region.c, iso.c = regnames)
# divide by 100 to get on proportion scale
#demand.ct <- cppred[,paste0("DataValue.", startyear:endyear)]/100

# also add CPR

cpr <- read.csv(cprestimates.dir)
temp<-cpr$cpr
cpr$cpr<-(temp-mean(temp))/sd(temp)
cpr.ct <- matrix(NA,C, nyears)
for (c in 1:C){
  for (t in 1:nyears){
    cpr.ct[c,t] <- cpr[cpr$ISO.code==iso.c[c] & cpr$year==seq(startyear, endyear)[t], "cpr"]  
  }
}
# also add CPR
demand <- read.csv(demandestimates.dir)
# temp<-demand$demand
# demand$demand<-(temp-mean(temp))/sd(temp)
demand.ct <- matrix(NA,C, nyears)
for (c in 1:C){
  for (t in 1:nyears){
    demand.ct[c,t] <- demand[demand$ISO.code==iso.c[c] & demand$year==seq(startyear, endyear)[t], "demand"]  
  }
}
demand.ct <- scale(demand.ct)
#summary(c(cpr.ct))

# which countries are not given in demand estimates?
select <- which(!is.element(paste(df$ISO.code), paste(iso.c)) &
                  !is.element(paste(df$ISO.code), paste(iso.c))  )
if (length(select)>0){
  df[select,] # small countries, indeed not included in FPET
  df <- df[-select, ]
}
getc.i <- Getc.i(iso.i = paste(df$ISO.code), iso.c = paste(iso.c))
# do we need this?
region.i <- region.c[getc.i]
getr.i <- getr.c[getc.i]


# edit LA: floor to get calendar year! 
calyear.i = floor(df$mid.year) # actually, already floored earlier!
gett.i = calyear.i - startyear + 1 
# if you are in year 1970 you are at t = 1 ... 1970 - 1970 + 1 = 1 

# c means contr user, u = unmet, s and l for spacing and limiting
cl <- df$G1.1_Using_Limit
cs <- df$G1.2_Using_Space
ul <- (df$G2.1_Unmet_Limit+df$G4.1_Unmet_Limit)
us <- (df$G2.2_Unmet_Space+df$G4.2_Unmet_Space)
unmet <- ul+us
totlimit <- cl+ul
totspace <- cs+us
totdemand <- cl+cs+ul+us
luup <- totlimit/totdemand
ratio <- (cl/totlimit)/(cs/totspace)
lup <- ul/unmet
# use luup and lup in data model
luup.samplesize <- totdemand
lup.samplesize <- unmet

GetSD <- function(p, samplesize){ #standard error for population stat rather than standard deviation
  return(sqrt(p*(1-p)/samplesize))
}
selup <- GetSD(p = lup, samplesize = lup.samplesize)
summary(selup)
seluup <- GetSD(p = luup, samplesize = luup.samplesize)
summary(seluup)
n <- length(luup)
ratio.i = ratio
demand.i <- rep(NA, n)
for (i in 1:n) demand.i[i] <- demand.ct[getc.i[i], gett.i[i]]

############### Spline Block ####################
temp <- seq(1,C*nyears,1)
getj.ct <- matrix(temp, nrow = C, ncol = nyears, byrow = TRUE)
demand.j <- as.vector(t(demand.ct))
I <- 2.5
res <- GetSplines(demand.j, I = I, x0 = max(demand.j) - 0.5 * I)
B.jk <- res$B.ik
K <- dim(B.jk)[2]
# the above does not work, can't have splines extend past y.i?
I <- 2.5
res <- GetSplines(demand.i, I = I, x0 = max(demand.i) - 0.5 * I)
B.ik <- res$B.ik
K <- dim(B.ik)[2]
###############################################

jagsdata <- list(lup.i = lup, luup.i = luup,
                 selup.i = selup, seluup.i = seluup, 
                 getc.i = getc.i, gett.i = gett.i, n = n,
                 demand.ct = demand.ct,
                 cpr.ct = cpr.ct, 
                 C  = C, R = R, 
                 getr.c = getr.c,
                 nyears = nyears,
                 regnames = regnames, name.c = name.c, iso.c = iso.c,
                 K = K, B.jk = B.jk, getj.ct = getj.ct, B.ik = B.ik)
if (!do.validation){
  # all observations are in the training set
  jagsdata$getitrain.j <- seq(1, n)
} else {
  # this is one particular type of validation, 
  # leaving out the most recent data point in all countries with at least 2 observations
  indiceslastobs.c <- rep(NA, C)
  for (c in 1:C){
    if (sum(getc.i==c)>2){
      indiceslastobs.c[c] <- which(gett.i==max(gett.i[getc.i==c]) & getc.i==c)   
    }
  }
  jagsdata$getitrain.j <- seq(1,n)[!is.element(seq(1,n), indiceslastobs.c[!is.na(indiceslastobs.c)])]
}
jagsdata$ntrain <- length(jagsdata$getitrain.j)
jagsdata$ntrain/n


jagsdata$input1.ct <- matrix(1, C, nyears)
jagsdata$cpr.ct - jagsdata$demand.ct
cpr.i <- rep(NA, n)
for (i in 1:n) cpr.i[i] <- cpr.ct[getc.i[i], gett.i[i]]
ds.i <- cpr.i/demand.i


