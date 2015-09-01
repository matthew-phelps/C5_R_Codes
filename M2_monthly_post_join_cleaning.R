# Author: Char Tamason & Matthew Phelps
# Desc:    Cleaning of entire dataset using X-2 file 
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, X2_data_cleaning and A1 to have been run


# Intro -------------------------------------------------------------------




# Prepare Matthew's workspace if user == MATTHEW. If else, do nothing
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()))
ifelse(grepl("zrc340", getwd()),
       baseline.path <- "CHAR - PUT PATH TO /Rdata HERE",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       ODK.path <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK",
       ODK.path <-"C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "CHAR - PUT PATH TO /Rdata HERE",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\")



## What is this from? Can we stop using A1??
x2 <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\X2_cleaned.Rdata"
x2.ct <- "PATH HERE"
setwd(mp)
rm(mp, ct)

library(dplyr)



# GLOBAL VARIABLES --------------------------------------------------------

# Go to this code to add or remove variables of interest


# 1.) LOAD DATA -----------------------------------------------------------

load(baseline.path)
load(x2)
x2 <- a5
rm(a5)


# 2.) REMOVE COLUMNS ------------------------------------------------------

# Re-order columns so Date visit fields are first.



# Subset monthlyAll to make it easier to work with
m <- MonthlyAll[, c('visitdate', 'hh_id', "FRA")]

# make x-2 an integet so ani_join can work later on
x2$HHID <- as.integer(x2$HHID)


# 3.) CHECK DUPLICATE RECORDS ---------------------------------------------

# Find observations where HH was recorded as being visited twice on same date:
x <- (duplicated(MonthlyAll[, c('visitdate', 'hh_id')]))
y <- which(x %in% T) #Gives index of duplicates
duplicates.df <- MonthlyAll[c(y, y-1), ] # gives df of duplicates. y-1 makes sure we get the
# 'original' and the 'duplicate'
rm(x, y)


# Remove hhid 185 (row 41)
MonthlyAll = MonthlyAll[-41,]
#need to delete row 237 or 109 depending on word from Bangladesh



# 4.) COMBINE TO X-2 ------------------------------------------------------

hhCleanup <- function(x) {
  # separates HHs that moved within the same compound so had two baselines but same HHID and same listing No.
  m2 <- data.frame(1,2,3)
  setnames(x2, old = c(1,2,3), new = c(colnames(x)))
  for(i in 1:nrow(x))  
    if (x$date.monthly.visit[i] >= x$Date.baseline[i] &
        x$date.monthly.visit[i] <= x$Date.withdrawl.move[i] ) {
      x2[i,] <- x[i,]
    } else {
      x2[i,] <- NA
    }
  x2$date.monthly.visit <- as.Date(x2$date.monthly.visit, origin = "1970-01-01")
  x2$Date.baseline <- as.Date(x2$Date.baseline, origin = "1970-01-01")
  x2$Date.phone.distribution <- as.Date(x2$Date.phone.distribution, origin = "1970-01-01")
  x2$Date.withdrawl.move <- as.Date(x2$Date.withdrawl.move, origin = "1970-01-01")
  return (x2)
}


x3 <- merge(m, x2, by.x = c("hh_id", 'visitdate'), by.y = c('HHID', 'date.monthly.visit'), all = T)

not.in.monthlyAll <- anti_join(x2, m, by = c('HHID' = "hh_id", "date.monthly.visit" = "visitdate"))
not.in.x2 <- anti_join(m, x2, by = c('hh_id' = "HHID", "visitdate" = "date.monthly.visit"))
