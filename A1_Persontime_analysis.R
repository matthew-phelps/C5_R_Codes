# Author: Matthew Phelps
# Desc:    Analysis of PT
# output: Analysis
# DEPENDENCIES: Requires M1-M5 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())

ifelse(grepl("zrc340", getwd()),
       pt <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\person-time.Rdata",
       pt <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")

library(dplyr)
library(data.table)
#detach("package:plyr", unload=TRUE) # disrupts the dplyr package


# FUNCTIONS ---------------------------------------------------------------


# GLOBAL VARIABLES  & FUNCTIONS--------------------------------------------------------
endDate <- as.Date('31-12-14', "%d-%m-%y")
#endDate <- Sys.Date()


# LOAD DATA ---------------------------------------------------------------

load(pt)
rm(pt)




# RESTRICT DATE -----------------------------------------------------------

a1 <- m5[m5$phone.dist <= endDate, ]

# Set withdraw data as end date for any records where true with draw is after end date
a1$with_date[a1$with_date > endDate] <- endDate

min(a1$with_date)
max(a1$with_date)


# Sum PT ------------------------------------------------------------------

pt.days <- sum(a1$pt)
pt.years <- pt.days/365
