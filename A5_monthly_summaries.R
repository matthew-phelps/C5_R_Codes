# Author: Matthew Phelps
# Desc:    Monthly summaries for Leela. Record for each month:
#   * how many actiev HHs
#   * how many new phones distributed
#   * number of active participants
#   * number of monthly visits undertaken
# output: summary data
# DEPENDENCIES: Requires M1-M5 & B1, B2 to have been run


# Intro -------------------------------------------------------------------

library(dplyr)
library(lubridate)
#detach("package:plyr", unload=TRUE) # disrupts the dplyr package

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())

ifelse(grepl("zrc340", getwd()),
       pt <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\person-time.Rdata",
       pt <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")




# LOAD DATA ---------------------------------------------------------------

load(pt)
source(functions.path)


# DATA PREPERATION -----------------------------------------------------

m5$VistMonthYear <- floor_date(m5$date_visit, unit = "month")
m5$phoneMonthYear <- floor_date(m5$phone.dist, unit = "month")
m5$withMonthYear <- floor_date(m5$with_date, unit = "month")

uniqueHH <- m5 %>%
  group_by(uniqueID) %>%
  summarise(
    active_ppl = mean(ppl)
  )

uniqueHH <- merge(uniqueHH, m5, by = "uniqueID")
x <- duplicated(uniqueHH[, 1])
uniqueHH <- uniqueHH[!x,]

# Remove variables that don't make sense for the aggregated uniqueHH dataframe:
uniqueHH$ppl <- uniqueHH$pt <- uniqueHH$date_visit <- uniqueHH$HHID <- NULL

# NEW PHONES Per MONTH ----------------------------------------------------------
# Counting how many phones were distributed each month

newPhones <- as.data.frame(table(uniqueHH$phoneMonthYear))

