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

x1 <- read.csv("X-2 monthly visits 31Jul15.csv", sep=",")

# remove rows without a HHID
x1 <- x1[complete.cases(x1$HHID),]


# Format HHID and Listing number to have standard length. Then create unique ID
x1$HHID <-formatC(x1$HHID, width = 3, format = 'd', flag = 0)
x1$Listing.number <- formatC(x1$Listing.number, width = 4, format = 'd', flag = 0)

# create unique ID called "unique_HHID_list_no"
x1$unique_ID <- paste(x1$HHID, x1$Listing.number, sep="-")


# turn date characters into date format
x1$Date.of.monthly.visit <- as.Date(x1$Date.of.monthly.visit, format = "%d.%m.%y")
