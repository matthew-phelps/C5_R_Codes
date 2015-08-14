# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
pc <- "C:/Users/wrz741/Dropbox/C5_R_Codes/Rdata"
setwd(pc)
rm(pc)

library(plyr)
library(xlsx)
CurrentDate <- function(x) {
  if (!is.na(x)) {
    return(x)
  } else {
    x <- endDate
    return(x)
  }
}

load("X-1 Choleraphone distribution 31Jul15.Rdata")
load("X-2 monthly visits 31Jul15.Rdata")
endDate <- as.Date('31-12-14', "%d-%m-%y")



# RESTRICT TO DESIRED TIME-FRAME

a1 <- x1[x1$Date.of.phone.distribution <= endDate, ]


# Calculate number of days each unique household is active
for (i in 1:nrow(a1)) {
a1$Date.of.withdrawl.or.move[i] <- CurrentDate(a1$Date.of.withdrawl.or.move[i])
}
a1$daysActive <- as.numeric(a1$Date.of.withdrawl.or.move - a1$Date.of.phone.distribution)
