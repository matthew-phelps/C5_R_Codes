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

x2 <- read.csv("X-2 monthly visits 31Jul15.csv", sep=",")

# remove rows without a HHID
x2 <- x2[complete.cases(x2$HHID),]


# Format HHID and Listing number to have standard length. Then create unique ID
x2$HHID <-formatC(x2$HHID, width = 3, format = 'd', flag = 0)
x2$Listing.number <- formatC(x2$Listing.number, width = 4, format = 'd', flag = 0)

# create unique ID called "unique_HHID_list_no"
x2$unique_ID <- paste(x2$HHID, x2$Listing.number, sep="-")


# turn date characters into date format
x2$Date.of.monthly.visit <- as.Date(x2$Date.of.monthly.visit, format = "%d.%m.%y")

save(x2, file = "C:/Users/wrz741/Dropbox/C5_R_Codes/Rdata/X-2 monthly visits 31Jul15.Rdata")
