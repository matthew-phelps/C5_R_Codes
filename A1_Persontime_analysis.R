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
setwd("/Users/Matthew/Dropbox (Personal)/C5_R_Codes/Rdata")

# FUNCTIONS ---------------------------------------------------------------
source(functions.path)

# GLOBAL VARIABLES  & FUNCTIONS--------------------------------------------------------
endDate <- as.Date('31-12-15', "%d-%m-%y")
startDate <- as.Date('2015-06-10')
#endDate <- Sys.Date()


# LOAD DATA ---------------------------------------------------------------
# Matthew to remove these two lines after finishes working from his Mac
source("/Users/Matthew/GitClones/C5_R_Codes/c_5_functions_source_file.R")
load("person-time.Rdata")

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


# CRUDE PT CALCULATION AFTER CHANGE IN DIARRHEA DEFINtion----------------------
# Instead of calculation PT for each household for each month, calculate it
# from the beginign of data to to end of data set

# Re-assign start dates of HH that received phone before change in diarrhea
# definition
a1_new_start_date <- a1

# Store index of obs that met condition:
cond1 <- a1_new_start_date$phone.dist < startDate

# For obs that met condition, replace old start date with date definition changed
a1_new_start_date$phone.dist[cond1] <- startDate
a1_new_start_date$base_date[cond1] <- startDate


# # If withdraw date is before startDate, remove records from analysis:
cond2 <- a1_new_start_date$with_date >= startDate
a2_new_start <- a1_new_start_date[cond2, ]

# Calculate new PT
a2_new_start <- (ptCalc(a2_new_start,
                       start.date = startDate,
                       end.date = endDate))
pt48.df <- pt48hr(a1_new_start_date,
                  start.date = startDate,
                  end.date = endDate)



pt.days <- sum(a2_new_start$pt)
pt.years <- pt.days/365
pt.years
pt48.days <- sum(pt48.df$pt48hr)
pt48.years <- pt48.days/365
pt48.years

