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
       baseline.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")




# LOAD DATA ---------------------------------------------------------------

load(baseline.path)
source(functions.path)


# DATA PREPERATION -----------------------------------------------------

base_merge$phoneMonthYear <- floor_date(base_merge$phone.dist, unit = "month")
base_merge$withMonthYear <- floor_date(base_merge$with_date, unit = "month")

# uniqueHH <- base_merge %>%
#   group_by(uniqueID) %>%
#   summarise(
#     active_ppl = mean(ppl)
#   )
# 
uniqueHH <- base_merge[, c(1,5,6,7,256,257)]
x <- duplicated(uniqueHH[, 1])
uniqueHH <- uniqueHH[!x,]

# Remove variables that don't make sense for the aggregated uniqueHH dataframe:
#uniqueHH$ppl <- uniqueHH$pt <- uniqueHH$date_visit <- uniqueHH$HHID <- NULL


# NEW PHONES PER MONTH ----------------------------------------------------------
# Counting how many phones were distributed each month

monthly_summary <- as.data.frame(table(uniqueHH$phoneMonthYear))
monthly_summary$Var1 <- as.Date(monthly_summary$Var1)



# ACTIVE HH and PEOPLE EACH MONTH ------------------------------------------------
x <- uniqueHH$withMonthYear - uniqueHH$phoneMonthYear
uniqueHH$int <- as.interval(x, uniqueHH$phoneMonthYear)


month.names <- strftime(monthly_summary$Var1, format = "%b-%Y")
z <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(uniqueHH)))
zx <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(uniqueHH)))
zy <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(uniqueHH)))
names(z) <- month.names
names(zx) <- month.names
names(zy) <- month.names
# Check if each month, during range of study period, is within the "active interval"
# for each household.
for (i in 1:nrow(monthly_summary)){
  for (j in 1:nrow(uniqueHH)){
   z[j, i] <- monthly_summary$Var1[i] %within% uniqueHH$int[j]
   #zx[j, i] <- round((monthly_summary$Var1[i] %within% uniqueHH$int[j]) * uniqueHH$active_ppl[j], digits = 0)
   zy[j, i] <- monthly_summary$Var1[i] == uniqueHH$withMonthYear[j]
  }
}
uniqueHH$withMonthYear[4] == monthly_summary$Var1[9]

z1 <- colSums(z)
zx1 <- colSums(zx)
zy1 <- colSums(zy)
monthly_summary$active_hh <- z1
monthly_summary$active_ppl <- zx1
monthly_summary$dropouts <- zy1
