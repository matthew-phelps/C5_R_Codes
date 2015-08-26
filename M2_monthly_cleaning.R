# Author: Char Tamason & Matthew Phelps
# Desc:    Join most recent ODK data table and merge with older versions of survey
# output: Joined ODK table?


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mp <- "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK"
ct <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK"
data.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata"
setwd(mp)
rm(mp, ct)


# 1.) SOURCE DATE JOIN CODE -----------------------------------------------

# Go to this code to add or remove variables of interest
source("C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\M1_monthly_joins.R")


# 2.) REMOVE COLUMNS ------------------------------------------------------

# Re-order columns so Date visit fields are first.
y <- match(c('visitdate', 'visitdateauto'), names(MonthlyAll))
x <- 1:(ncol(MonthlyAll) - length(y))
MonthlyAll <- MonthlyAll[, c(y, x)]


# Remove duplicate columns.
dropVar <- c('x', 'month_auto', 'day_auto', 'y', 'z', 'year_auto', 'auto_date',
             'day', 'month', 'year')
MonthlyAll <- MonthlyAll[, !names(MonthlyAll) %in% dropVar]
rm(y,x, dropVar)
MonthlyAll$visitdateauto[1]



# 1.) CHECK DUPLICATE RECORDS ---------------------------------------------

# Find observations where HH was recorded as being visited twice on same date:
x <- (duplicated(MonthlyAll[, c('visitdate', 'hh_id')]))
y <- which(x %in% T) #Gives index of duplicates
duplicates.df <- MonthlyAll[c(y, y-1), ] # gives df of duplicates. y-1 makes sure we get the
# 'original' and the 'duplicate'
rm(x, y)


# Time between human recorded visit date and auto recorded visit date:
x <- as.data.frame(MonthlyAll$visitdate - MonthlyAll$visitdateauto)



# Remove hhid 185 (row 41)
MonthlyAll = MonthlyAll[-41,]
#need to delete row 237 or 109 depending on word from Bangladesh


# 2.) PRE-CLEANING CHECK DATES ---------------------------------------------------------

# Top 10 latest dates:
tail(sort(MonthlyAll$visitdate), 10)
# Top 10 earliest dates:
head(sort(MonthlyAll$visitdate), 10)

# Select records that are before start date or after end date (which is today's system date)
early <- MonthlyAll[MonthlyAll$visitdate < start.date, ]
late <- MonthlyAll[MonthlyAll$visitdate > end.date, ]


# 3.) CLEANING ------------------------------------------------------------

# "2014-04-07" and "2014-08-15" are earlier, "2016-02-18" "2016-05-11" are later
# MonthlyAll$visitdateauto[MonthlyAll$visitdate=="2014-04-07"] # is "2015-04-07", change it
MonthlyAll$visitdate[MonthlyAll$visitdate == "2014-04-07"] <- as.Date("2015-04-07")
MonthlyAll$visitdate[MonthlyAll$visitdate == "2014-08-15"] <- as.Date("2014-09-15")
MonthlyAll$visitdate[MonthlyAll$visitdate == "2016-02-18"] <- as.Date("2015-02-18")
MonthlyAll$visitdate[MonthlyAll$visitdate == "2016-05-11"] <- as.Date("2015-05-11")

# Check with Char about these. The Auto date was exactly 1-yr before the recorded date:
MonthlyAll$visitdate[MonthlyAll$visitdate == "2016-07-13"] <- as.Date("2015-07-13")
MonthlyAll$visitdate[MonthlyAll$visitdate == "2016-07-09"] <- as.Date("2015-07-09")

hist(MonthlyAll$visitdateauto , breaks = 20)

# POST-CLEANING CHECK -----------------------------------------------------

# Top 10 latest dates:
tail(sort(MonthlyAll$visitdate), 10)
# Top 10 earliest dates:
head(sort(MonthlyAll$visitdate), 10)

# Select records that are before start date or after end date (which is today's system date)
early <- MonthlyAll[MonthlyAll$visitdate < start.date, ]
late <- MonthlyAll[MonthlyAll$visitdate > end.date, ]
