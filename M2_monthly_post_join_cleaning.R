# Author: Char Tamason & Matthew Phelps
# Desc:    Join most recent ODK data table and merge with older versions of survey
# output: Joined ODK table?


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mp <- "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK"
ct <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK"
data.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata"
data.path.ct <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data"

setwd(mp)
rm(mp, ct)


# GLOBAL VARIABLES --------------------------------------------------------

# Go to this code to add or remove variables of interest
load(data.path)


# 2.) REMOVE COLUMNS ------------------------------------------------------

# Re-order columns so Date visit fields are first.
y <- match(c('visitdate'), names(MonthlyAll))
x <- 1:(ncol(MonthlyAll) - length(y))
MonthlyAll <- MonthlyAll[, c(y, x)]


# Remove duplicate columns.
dropVar <- c('x', 'month_auto', 'day_auto', 'y', 'z', 'year_auto', 'auto_date',
             'day', 'month', 'year')
MonthlyAll <- MonthlyAll[, !names(MonthlyAll) %in% dropVar]
rm(y,x, dropVar)
MonthlyAll$visitdate[1]



# 1.) CHECK DUPLICATE RECORDS ---------------------------------------------

# Find observations where HH was recorded as being visited twice on same date:
x <- (duplicated(MonthlyAll[, c('visitdate', 'hh_id')]))
y <- which(x %in% T) #Gives index of duplicates
duplicates.df <- MonthlyAll[c(y, y-1), ] # gives df of duplicates. y-1 makes sure we get the
# 'original' and the 'duplicate'
rm(x, y)


# Remove hhid 185 (row 41)
MonthlyAll = MonthlyAll[-41,]
#need to delete row 237 or 109 depending on word from Bangladesh



