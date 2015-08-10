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
x1_2 <- lapply(x1[,3:5], as.Date, "%d.%m.%y")

# format dates to be more human readible
x1_2 <- lapply(x1_2, format, "%d %b %Y")

# merge back into original dataset
x2 <- cbind(x1[1:2], x1_2[1], x1_2[2], x1_2[3])
