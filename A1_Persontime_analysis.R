# Author: Matthew Phelps
# Desc:    Person-time calculations
# output: Person-time data for each hosehold
# DEPENDENCIES: Requires M1-M5 & B1, B2 to have been run


# Intro -------------------------------------------------------------------
library(dplyr)
library(data.table)

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())

ifelse(grepl("zrc340", getwd()),
       pt <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\person-time.Rdata",
       pt <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")


#detach("package:plyr", unload=TRUE) # disrupts the dplyr package


# FUNCTIONS ---------------------------------------------------------------
source(functions.path)

# GLOBAL VARIABLES  & FUNCTIONS--------------------------------------------------------
endDate <- as.Date('31-12-14', "%d-%m-%y")
#endDate <- Sys.Date()


# LOAD DATA ---------------------------------------------------------------

load(pt)
rm(pt)


# RESTRICT DATE -----------------------------------------------------------

# Remove records where phone was distributed after the specified endDate
a1 <- m5[m5$phone.dist <= endDate, ]

# Set withdraw date as end date for any records where true withdraw is after end date
a1$with_date[a1$with_date > endDate] <- endDate

min(a1$with_date)
max(a1$with_date)


# CALCULATE PT BASED ON CUSTOM END DATE ------------------------------------------------------------------
a2 <- ptCalc(a1, end.date = endDate)
pt48.df <- pt48hr(a1, end.date = endDate)

pt.days <- sum(a2$pt)
pt.years <- pt.days/365

pt48.days <- sum(pt48.df$pt48hr)
pt48.years <- pt48.days/365