# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()) + NA)
ifelse(grepl("zrc340", getwd()),
       m3 <- "CHAR - PUT PATH TO CLEANED Monthly-Joined (M2 Output) HERE",
       m3 <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-baseline_join.Rdata")

library(dplyr)
library(data.table)
#detach("package:plyr", unload=TRUE) # disrupts the dplyr package


# FUNCTIONS ---------------------------------------------------------------
CustomEndDate <- function(x) {
  # Returns the specified end date if the withdrawl field is null
  if (x < endDate & !is.na(x)){
      return(x)
  } else {
    x <- endDate
    return(x)
  }
}
hhCleanup <- function(x) {
  # separates HHs that moved within the same compound so had two baselines but same HHID and same listing No.
  x2 <- data.frame(1,2,3,4,5,6,7,8,9)
  setnames(x2, old = c(1,2,3,4,5,6,7,8,9), new = c(colnames(x)))
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
hhCleanup.monthlyAll <- function(x) {
  # separates HHs that moved within the same compound so had two baselines but same HHID and same listing No.
  x2 <- data.frame(1,2,3,4,5,6,7,8,9)
  setnames(x2, old = c(1,2,3,4,5,6,7,8,9), new = c(colnames(x)))
  for(i in 1:nrow(x))  
    if (x$visitdate[i] >= x$Date.baseline[i] &
        x$visitdate[i] <= x$Date.withdrawl.move[i] ) {
      x2[i,] <- x[i,]
    } else {
      x2[i,] <- NA
    }
  
  
  
  x2$visitdate <- as.Date(x2$visitdate, origin = "1970-01-01")
  x2$Date.baseline <- as.Date(x2$Date.baseline, origin = "1970-01-01")
  x2$Date.phone.distribution <- as.Date(x2$Date.phone.distribution, origin = "1970-01-01")
  x2$Date.withdrawl.move <- as.Date(x2$Date.withdrawl.move, origin = "1970-01-01")
  return (x2)
}
personTime <- function(x) {
  # Calculates person time for each specified date range. Returns data frame in same dimensions as input
  complete.x <- x[complete.cases(x),]
  factor = complete.x$HH_baseline
  x1 <- split(complete.x, factor)
  for (j in 1:length(x1)) {
    x1[[j]]$pt <-0
    x1[[j]]$delay <-0
    #if (x1[[j]]$HH_baseline == "") {browser()} #For debugging
    for (i in 1:nrow(x1[[j]])) {
      
      if(i == 1 & nrow(x1[[j]]) != 1 & x1[[j]]$date.monthly.visit[i] > x1[[j]]$Date.phone.distribution[i] ) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$date.monthly.visit[i] - x1[[j]]$Date.phone.distribution[i]) * x1[[j]]$Num_ppl[i]
      } else if (i == 1 & nrow(x1[[j]]) != 1) {
        x1[[j]]$pt <- 0
      } else if (nrow(x1[[j]]) == 1 & x1[[j]]$Date.withdrawl.move[i] > x1[[j]]$Date.phone.distribution[i]) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$Date.withdrawl.move[i] - x1[[j]]$Date.phone.distribution[i]) * x1[[j]]$Num_ppl[i]
      } else if(i > 1 & i < nrow(x1[[j]]) & x1[[j]]$date.monthly.visit[i] > x1[[j]]$date.monthly.visit[i-1]) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$date.monthly.visit[i] - x1[[j]]$date.monthly.visit[i-1]) * x1[[j]]$Num_ppl[i]
      } else if (x1[[j]]$Date.withdrawl.move[i] >= x1[[j]]$date.monthly.visit[i]) {
        x1[[j]]$pt[i] <- as.numeric((x1[[j]]$Date.withdrawl.move[i] - x1[[j]]$date.monthly.visit[i]) +
                                      (x1[[j]]$date.monthly.visit[i] - x1[[j]]$date.monthly.visit[i-1])) * x1[[j]]$Num_ppl[i]
      } else {
        x1[[j]]$pt <- 'ERROR'
      }
      x1[[j]]$delay <- min(x1[[j]]$date.monthly.visit - x1[[j]]$Date.phone.distribution )
    }
  } 
  x1 <-  do.call(rbind.data.frame, x1)
  row.names(x1) <- NULL
  return(x1)
}


# GLOBAL VARIABLES --------------------------------------------------------
endDate <- as.Date('31-12-14', "%d-%m-%y")
endDate <- Sys.Date()



# LOAD DATA ---------------------------------------------------------------

load(m3)
m3$wi

# SUBSET VARIABLES --------------------------------------------------------
m3 <- m3[, c("uniqueID", 'HHID', 'date_visit', 'ppl', 'base_date', 'phone.dist', 'with_date' )]


# Rename variables for less space.
x2 <- rename(x2, Num_ppl = Numer.of.ppl.in.household.at.monthly.visit,
             date.monthly.visit = Date.of.monthly.visit)
x1 <- rename(x1, Date.baseline = Date.of.baseline, Date.phone.distribution = Date.of.phone.distribution,
             Date.withdrawl.move = Date.of.withdrawl.or.move)
x3 <- MonthlyAll[, c('visitdate', 'hh_id')]

x3[x3$hh_id==331,]


# Change HHID to integer
x1$HHID <- as.integer(x1$HHID)
x2$HHID <- as.integer(x2$HHID)

# RESTRICT TO DESIRED TIME-FRAME

a1 <- x1[x1$Date.phone.distribution <= endDate, ]
rm(x1)

a2 <- x2[x2$date.monthly.visit <= endDate,]
rm(x2)

# SET WITHDRAWL DATE AS END DATE FOR HOUSEHOLDS CURRENTLY STILL ENROLLED
for (i in 1:nrow(a1)) {
a1$Date.withdrawl.move[i] <- CustomEndDate(a1$Date.withdrawl.move[i])
}
min(a1$Date.withdrawl.move)
max(a1$Date.withdrawl.move)
# a1$daysActive <- as.numeric(a1$Date.of.withdrawl.or.move - a1$Date.of.phone.distribution)


# MERGE x1, x2 - Keep all records at first to find missing data
a3 <- merge(a2, a1, by ="hh_listing_id", all=T, suffixes = c("", ".y"))

a3$HHID.y <- a3$Listing.number.y <- NULL

# Records for which we have no monthly visit data
missing.df <- a3[!complete.cases(a3), ] 


# CLEAN where house moved but kept same HHID and listing number
a4 <- hhCleanup(a3[complete.cases(a3), ])

# PERSONE TIME for each household during each time-frame
a5 <- personTime(a4)

er <- a5[a5$delay <0,]
rm(a3)



# ERROR CHECKING ----------------------------------------------------------
# 
min(a5$pt, na.rm=T)
which.max(a5$pt)
# 
# # Records for which the HHID and Listing number appear twice.
# # Maybe same phone given to someone else in compound, or same family move
# # within compound
error.df <- a5[a5$pt=='ERROR',]
error.df <- error.df[complete.cases(error.df), ]
# 
 lateVisits <- a5[a5$delay > 60, ] %>%
   group_by(HH_baseline) %>%
   summarize( delay = mean(delay))

 
 
# a5[a5$delay > 60, ]
# # PERSON-TIME CALCULATIONS ------------------------------------------------
# 
# a6 <- a5[complete.cases(a5) & a5$pt != "ERROR", ]
# a6$pt <- as.numeric(a6$pt)
# sum(a6$pt)
# sum(a6$pt) /365
# 
# 
# # Checking data
# min(a6$pt)
# max(a6$pt)
# which.max(a6$pt)
# 
# hist(a6$pt)
# boxplot(a6$pt)


# WRITE TO FILE -----------------------------------------------------------
# 
save(a5, file = "X2_cleaned.Rdata")
# write.csv(missing.data.df[,c(1,6,7,8)], file = "missing_data.csv")
# write.csv(a6, file = "A-1 Person-Time per HH.csv")
# write.csv(error.df, file = "A-3 Data Checking.csv")

 
 
 
 
# MERGE X1, MONTHLYALL ----------------------------------------------------

 
 
month_baseline <- merge(a1, MonthlyAll, by.x = "HHID", by.y = "hh_id", all = T)
month_baseline <- month_baseline[, 1:9]
 
 
# Remove HHID where the HHID and listing number were used for multiple baselines
y <- hhCleanup.monthlyAll(month_baseline)
y <- y[complete.cases(y),]


# Df of baselines that were NOT match with a monthly visit record
z0 <- anti_join(y, MonthlyAll, by = c("HHID" = "hh_id", "visitdate" = "visitdate"))
# Df of monthly visit records that were NOT matched with a baseline 
z <- anti_join(MonthlyAll, y, by = c("hh_id" = "HHID", "visitdate" = "visitdate"))


## hhid 302: Incorrect recording of phone distribution for listing number 1318