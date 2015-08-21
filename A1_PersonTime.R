# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
pc <- "C:/Users/wrz741/Dropbox/C5_R_Codes/Rdata"
setwd(pc)
rm(pc)

library(dplyr)
library(xlsx)
CustomEndDate <- function(x) {
  # Returns the specified end date if the withdrawl field is null
  if (x < endDate & !is.na(x)){
      return(x)
  } else {
    x <- endDate
    return(x)
  }
}
personTime <- function(x) {
  # Calculates person time for each specified date range
  complete.x <- x[complete.cases(x),]
  factor = complete.x$unique_ID
  x1 <- split(complete.x, factor)
  for (j in 1:length(x1)) {
    x1[[j]]$pt <-0
    x1[[j]]$delay <-0
    for (i in 1:nrow(x1[[j]])) {
      
      if(i == 1 & nrow(x1[[j]]) != 1 & x1[[j]]$date.monthly.visit[i] > x1[[j]]$Date.phone.distribution[i] ) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$date.monthly.visit[i] - x1[[j]]$Date.phone.distribution[i]) * x1[[j]]$Num_ppl[i]
      } else if (i == 1 & nrow(x1[[j]]) != 1) {
        x1[[j]]$pt <- 0
      } else if (nrow(x1[[j]]) == 1 & x1[[j]]$Date.withdrawl.move[i] > x1[[j]]$Date.phone.distribution[i]) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$Date.withdrawl.move[i] - x1[[j]]$Date.phone.distribution[i]) * x1[[j]]$Num_ppl[i]
      } else if(i > 1 & i < nrow(x1[[j]]) & x1[[j]]$date.monthly.visit[i] > x1[[j]]$date.monthly.visit[i-1]) {
        x1[[j]]$pt[i] <- as.numeric(x1[[j]]$date.monthly.visit[i] - x1[[j]]$date.monthly.visit[i-1]) * x1[[j]]$Num_ppl[i]
      } else if (x1[[j]]$Date.withdrawl.move[i] > x1[[j]]$date.monthly.visit[i]) {
        x1[[j]]$pt[i] <- as.numeric((x1[[j]]$Date.withdrawl.move[i] - x1[[j]]$date.monthly.visit[i]) +
                                      (x1[[j]]$date.monthly.visit[i] - x1[[j]]$date.monthly.visit[i-1])) * x1[[j]]$Num_ppl[i]
      } else {
        x1[[j]]$pt <- 'ERROR'
      }
      x1[[j]]$delay <- min(x1[[j]]$date.monthly.visit - x1[[j]]$Date.phone.distribution )
    }
  } 
  return(x1)
}


load("X-1 Choleraphone distribution 31Jul15.Rdata")
load("X-2 monthly visits 31Jul15.Rdata")
endDate <- as.Date('31-12-14', "%d-%m-%y")
endDate <- Sys.Date()

# Rename variables for less space.
x2 <- rename(x2, Num_ppl = Numer.of.ppl.in.household.at.monthly.visit,
             date.monthly.visit = Date.of.monthly.visit)
x1 <- rename(x1, Date.baseline = Date.of.baseline, Date.phone.distribution = Date.of.phone.distribution,
             Date.withdrawl.move = Date.of.withdrawl.or.move)


# RESTRICT TO DESIRED TIME-FRAME

a1 <- x1[x1$Date.phone.distribution <= endDate, ]
rm(x1)

a2 <- x2[x2$date.monthly.visit <= endDate,]
rm(x2)

# SET END DATE
for (i in 1:nrow(a1)) {
a1$Date.withdrawl.move[i] <- CustomEndDate(a1$Date.withdrawl.move[i])
}
min(a1$Date.withdrawl.move)
max(a1$Date.withdrawl.move)
# a1$daysActive <- as.numeric(a1$Date.of.withdrawl.or.move - a1$Date.of.phone.distribution)


# MERGE - Keep all records at first to find missing data
a3 <- merge(a2, a1, by ="unique_ID", all=T, suffixes = c("", ".y"))
a3$HHID.y <- a3$Listing.number.y <- NULL


# Records for which we have no monthly visit data
missing.df <- a3[!complete.cases(a3),] 


# PERSONE TIME for each household during each time-frame
a4 <- personTime(a3)

# Return to Data Frame for easier reading.
a5 <- do.call(rbind.data.frame, a4)
row.names(a5) <- NULL
rm(a3)




# ERROR CHECKING ----------------------------------------------------------

min(a5$pt, na.rm=T)
max(a5$pt, na.rm=T)

# Records for which the HHID and Listing number appear twice.
# Maybe same phone given to someone else in compound, or same family move
# within compound
error.df <- a5[a5$pt=='ERROR',]
error.df <- error.df[complete.cases(error.df),]

lateVisits <- a5[a5$delay > 60, ] %>%
  group_by(unique_ID) %>%
  summarize( delay = mean(delay))

a5[a5$delay > 60,]
# PERSON-TIME CALCULATIONS ------------------------------------------------

a6 <- a5[complete.cases(a5) & a5$pt != "ERROR",]
a6$pt <- as.numeric(a6$pt)
sum(a6$pt)
sum(a6$pt) /365


# Checking data
min(a6$pt)
max(a6$pt)
which.max(a6$pt)

hist(a6$pt)
boxplot(a6$pt)



# WRITE TO FILE -----------------------------------------------------------
# 
# 
# write.csv(missing.data.df[,c(1,6,7,8)], file = "missing_data.csv")
# write.csv(a6, file = "A-1 Person-Time per HH.csv")
# write.csv(error.df, file = "A-3 Data Checking.csv")
