# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- "/Users/Matthew/Google Drive/Copenhagen\\Bangladesh - Cholera\\Map Stuff"
pc <- "C:/Users/wrz741/Dropbox/C5 Baseline data/Pre-double entry"
setwd(pc)
rm(mac, pc)

library(plyr)
library(memisc) # read .spss files


HH_member_info<-as.data.set(spss.system.file('Q11.sav'), stringsAsFactors=FALSE)

# make it easier to work with in R. 
# Please change "x" to a variable name that makes sense for you!
x <-as.data.frame(HH_member_info) 

# make dummy variable for each age range so counting will be easier
x$child_U5 <- ifelse(x$q11_4 < 5, 1, 0)
x$child_5_17 <- ifelse(x$q11_4 >= 5 & x$q11_4 <18, 1, 0)
x$adult <- ifelse(x$q11_4 > 18, 1, 0)

# For each unique "slno" in df "x", sum the number of adults, children U5 and children 5-17.
# Change "slno" to "hhid" if you wish to count based on HHID.
x2 <- ddply(x, .(slno),
      summarize,
      adults = sum(adult),
      children_U5 = sum(child_U5),
      children_5_17 = sum(child_5_17))

# Add HHID variable to make our final dataset.
x3 <- merge(x[,c(1,10)], x2, by = "slno")

# check to make sure summation works as expected. Returns "TRUE" if it works
sum(x$child_U5) == sum(x3$children_U5)
sum(x$child_5_17)== sum(x3$children_5_17)
sum(x$adult)== sum(x3$adults)



