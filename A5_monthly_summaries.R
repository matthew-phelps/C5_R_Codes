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
       m5.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\person-time.Rdata",
       m5.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")




# LOAD DATA ---------------------------------------------------------------

load(baseline.path)
load(m5.path)
source(functions.path)


# DATA PREPERATION -----------------------------------------------------

base_merge$phoneMonthYear <- floor_date(base_merge$phone.dist, unit = "month")
base_merge$withMonthYear <- floor_date(base_merge$with_date, unit = "month")
m5$date_visit_month <- floor_date(m5$date_visit, unit = "month")

# uniqueHH <- base_merge %>%
#   group_by(uniqueID) %>%
#   summarise(
#     active_ppl = mean(ppl)
#   )
# 
uniqueHH <- select(base_merge, uniqueID, base_date.x, with_date, phone.dist, phoneMonthYear, withMonthYear)
x <- duplicated(uniqueHH[, 1])
uniqueHH <- uniqueHH[!x,]

# Remove variables that don't make sense for the aggregated uniqueHH dataframe:
#uniqueHH$ppl <- uniqueHH$pt <- uniqueHH$date_visit <- uniqueHH$HHID <- NULL


# NEW PHONES PER MONTH ----------------------------------------------------------
# Counting how many phones were distributed each month

monthly_summary <- as.data.frame(table(uniqueHH$phoneMonthYear))
monthly_summary$Var1 <- as.Date(monthly_summary$Var1)
monthly_summary <- rename(monthly_summary, new.phones = Freq)


# ACTIVE HH & DROPOUT HH ------------------------------------------------

# Create interval object. Use the date of phone distribution as the start of
# interval reference time
x <- uniqueHH$withMonthYear - uniqueHH$phoneMonthYear
uniqueHH$int <- as.interval(x, uniqueHH$phoneMonthYear)
x

month.names <- strftime(monthly_summary$Var1, format = "%b-%Y")
z <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(uniqueHH)))
zy <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(uniqueHH)))
names(z) <- month.names
names(zy) <- month.names

# Check if each month, during range of study period, is within the "active interval"
# for each household.
for (i in 1:nrow(monthly_summary)){
  for (j in 1:nrow(uniqueHH)){
    z[j, i] <- monthly_summary$Var1[i] %within% uniqueHH$int[j]
    # Number of HHs dropping out or moving 
    zy[j, i] <- monthly_summary$Var1[i] == uniqueHH$withMonthYear[j]
  }
}

# Number of individuals dropping out from HHs, but HHs remain in study
temp <- m5 %>%
  group_by(date_visit_month) %>%
  summarise(dropout_individuals = sum(old_per_out, na.rm = T))

# Merge into summary data.frame
z1 <- colSums(z)
zy1 <- colSums(zy)
monthly_summary$active_hh <- z1
monthly_summary$dropout_HHs <- zy1
monthly_summary <- left_join(monthly_summary, temp, by = c("Var1" = "date_visit_month"))

rm(z, zy, temp, z1, zy1, x, m5.path, i, j, baseline.path)


# ACTIVE PPL --------------------------------------------------------------

t <- split(m5, f = m5$uniqueID)
t.temp <- t[[2]]
activePeople <- function(t.temp) {
  
  # Create time interval b/w visits
  g <- NA
  x <- new_interval(t.temp[1,11], t.temp[1,11]) 
  # If there is >1 monthly visit:
  if (nrow(t.temp)> 1){
    for (i in 1:nrow(t.temp)-1){
      g[i] <- t.temp[i+1,11] - t.temp[i,11]
      x[i] <- as.interval(g[i], t.temp[i,11])
    }
  x[nrow(t.temp),] <- as.interval(t.temp$with_date[nrow(t.temp)] - t.temp$date_visit_month[nrow(t.temp)-1], t.temp$date_visit_month[nrow(t.temp)-1])
  
  # first interval - measure from phone.dist date to date of second monthly visit
  x[1,] <- as.interval(t.temp$date_visit_month[2] - floor_date(t.temp$phone.dist[1], unit = "month"), floor_date(t.temp$phone.dist[1], unit = "month"))
  } else { # if there is only 1 monthly visit, use interval from phone.dist to withdraw
    x <- as.interval((t.temp$with_date - t.temp$phone.dist), t.temp$phone.dist)
  }
  # associate each interval with the number of active ppl during that interval
  x <- as.data.frame(x)
  x <- rename(x, interval = x)
  t.temp <- cbind(t.temp, x)
  rm(x)
  
  # Figure out how many people are present during each time interval. Cuz there
  # are overlapping intervals, take the Max no. of ppl (no rational, just easier
  # than taking the min)
  h <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(t.temp)))
  h1 <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = 1))
  names(h1) <- month.names
  for (i in 1:nrow(monthly_summary)){
    for (j in 1:nrow(t.temp)){
      h[j, i] <- (monthly_summary$Var1[i] %within% t.temp$interval[j]) * t.temp$ppl_all[j]
    }
    h1[i] <- max(h[,i])
  }
  return(h1)
}

vf <- lapply(t, activePeople)
cv <- do.call(rbind.data.frame, vf)
month.temp <- colSums(cv)
monthly_summary <- cbind(monthly_summary, month.temp)
monthly_summary <- rename(monthly_summary, active_ppl = month.temp)
rm(vf, cv, month.temp)
