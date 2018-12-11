# LA: i read in demand first, because some ISOs in data don't show up in demand so we'll exclude those obs
# never mind... let's just include countries with data only in this test phase
# so frist read raw data and then keep those isos with data only
# Working with response data (unmet 2017 data)
# Import data
df <- read.csv(rawdata.dir)
# We start by getting the complete case for limiting (we look at other exclusion later)
df <- df[df$Unmet.need.limiting!="..",] # they use .. for NA so this is just my way of getting the complete case
# Create limiting proportion variable
# for some reason all the variables are factors so we change that here
df$Unmet.need.total <- as.numeric(as.character(df$Unmet.need.total))
df$Unmet.need.limiting <- as.numeric(as.character(df$Unmet.need.limiting))
df$LUP <- df$Unmet.need.limiting/df$Unmet.need.total

# Create mid year variable
df$mid.year <- floor((df$end_date+df$start_date)/2)
# We need to exclude some based on these notes
levels(df$Unmet.need..indicator)



# what is going on here? 
#unique(df$Unmet.need..indicator) # let's remove all except for Preliminary data (spelled differently)
#paste(unique(df$Unmet.need..indicator)[c(1, 3,7)])
select <- c("Preliminary data. ", "Preliminary data.", "")
mean(is.element(as.character(df$Unmet.need..indicator), select))
df <- df[is.element(as.character(df$Unmet.need..indicator), select),]

unique(df$"Unmet.need..population.included")
unique(df$"Unmet.need..population.excluded")
# let's remove all with a note for now
mean(df$"Unmet.need..population.included" !="" & df$"Unmet.need..population.excluded" !="")
df <- df[df$"Unmet.need..population.included" =="" & 
           df$"Unmet.need..population.excluded" =="",]

# Data pertain to unmet need for limiting only.
# Figures for spacing and limiting do not add up to the total.
#df <- df[as.character(df$Unmet.need..indicator)!="Data pertain to unmet need for limiting only.",]
#df <- df[as.character(df$Unmet.need..indicator)!="Figures for spacing and limiting do not add up to the total.",]

# let's also remove nondhs/nonmics for now
df <- df[is.element(df$Source, c("DHS", "MICS")),]
df$y <- df$LUP
# end data for LUP, gives mid.year and ISO.code and LUP