# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
pc <- "C:/Users/wrz741/Dropbox/C5 Field Operations data/Folder Amal"
setwd(pc)
rm(pc)

library(plyr)
library(xlsx)

x1 <- read.csv("X-1 Choleraphone distribution 31Jul15.csv", sep=";")

# remove rows without a HHID
x1 <- x1[complete.cases(x1$HHID),]

# turn date characters into date format
x1_2 <- lapply(x1[,3:5], as.Date, format = "%d.%m.%y")


# Populate the withdrawl/move date as the current date for calculation purposes.
# Use a custom function
CurrentDate <- function(x) {
  if (!is.na(x)) {
    return(x)
  } else {
    x <- Sys.Date()
    return(x)
    }
}

for (i in 1:nrow(x1)) {
  x1_2$Date.of.withdrawl.or.move[i] <- CurrentDate(x1_2$Date.of.withdrawl.or.move[i])
}

x2 <- cbind(x1[1:2], x1_2[1], x1_2[2], x1_2[3])
rm(x1, x1_2)



# Calculate number of days each unique household is active
x2$daysActive <- as.numeric(x2$Date.of.withdrawl.or.move - x2$Date.of.phone.distribution)


hist(x2$daysActive, breaks = 15)
