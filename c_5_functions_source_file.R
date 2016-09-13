# Author: Matthew Phelps
# Desc:    Functions for C-5 project
# output: Function objects
# DEPENDENCIES: none

# B2 functions ------------------------------------------------------------

moveInternal <- function(x) {
  # Check to see if household moved. If hh moved, use the withdaw date of previous
  # hh's entry as the phone.dist date to make sure we don't double count person time
  x <- x[order(x$base_date), ]
  x$phone.dist2 <- x$phone.dist
  x$moved <- F
  for (i in 1:nrow(x)) {
    if(nrow(x) > 1 && i < nrow(x) && x$phone.dist[i] == x$phone.dist[i+1]) {
      x$phone.dist2[i+1] <- x$with_date[i]
      x$moved <- T
    }
  }
  x$phone.dist <- x$phone.dist2
  x$phone.dist2 <- NULL
  return(x)
}

moveDates <- function (x, factor) {
  x.temp <- split(x = x, f = factor)
  z.temp <- lapply(x.temp, moveInternal)
  z1.temp <- do.call(rbind.data.frame, z.temp)
  return(z1.temp)
}



# M2 Functions ------------------------------------------------------------
dateReplace <- function (x) {
  # On each row, test to see if there is a ODK entry, and the entry before or
  # after was only a X2 entry
  # If row i is odk and row i+1 or i-1 is only x2 - take the date from row i and
  # assign it to row i+1 or i-1
  for (i in 1:nrow(x)){
    if(i> 1 && i < nrow(x) && is.na(x[i, ]$Listing.number) && (is.na(x[i-1, ]$FRA) | is.na(x[i+1, ]$FRA))){
      if(x[i, ]$date_visit <= (x[i-1, ]$date_visit + 2)) {
        x[i-1, ]$date_visit <- x[i, ]$date_visit
      } else if (x[i, ]$date_visit >= (x[i+1, ]$date_visit - 2)) {
        x[i+1, ]$date_visit <- x[i, ]$date_visit
      }
    } else if(i == 1 && nrow(x) >1 && is.na(x[i, ]$Listing.number) && is.na(x[i+1, ]$FRA) && x[i, ]$date_visit >= (x[i+1, ]$date_visit - 2)) {
      x[i+1, ]$date_visit <- x[i, ]$date_visit 
      
    } else if (i == nrow(x) && nrow(x) > 0 && is.na(x[i, ]$Listing.number) && is.na(x[i-1, ]$FRA) && x[i, ]$date_visit <= (x[i-1, ]$date_visit + 2)) {
      x[i-1, ]$date_visit <- x[i, ]$date_visit
    }
  }
  return (x)
}

mergeRows <- function(x) {
  for (i in 1:nrow(x)){
    if(i < (nrow(x)) && !is.na(x[i, ]$FRA) && (x[i, ]$date_visit == x[i + 1, ]$date_visit)) {
      x[i, ]$ppl <- x[i + 1, ]$ppl
      x[i, ]$Listing.number <- x[i + 1, ]$Listing.number
    } else if (i > 1 && !is.na(x[i, ]$FRA) && x[i, ]$date_visit == x[i - 1, ]$date_visit) {
      x[i, ]$ppl <- x[i - 1, ]$ppl
      x[i, ]$Listing.number <- x[i - 1, ]$Listing.number
    }
  }
  return (x)  
}



# M3 Functions ------------------------------------------------------------
hhCleanup <- function(data, dateVisit, baseDate, withdrawDate, phoneDate) {
  # separates records that have same HHID but different baselines.
  x2 <- data.frame(matrix(ncol=ncol(data), nrow = nrow(data)))
  names(x2) <- names(data)
  for(i in 1:nrow(data)) {
    if (data[i, dateVisit] >= data[i, baseDate] &&
        data[i, dateVisit] <= data[i, withdrawDate] ) {
      x2[i,] <- data[i,]
    }
  }
  x2$date_visit <- as.Date(x2$date_visit, origin = "1970-01-01")
  x2$base_date <- as.Date(x2$base_date, origin = "1970-01-01")
  x2$phone.dist <- as.Date(x2$phone.dist, origin = "1970-01-01")
  x2$with_date <- as.Date(x2$with_date, origin = "1970-01-01")
  x2 <- x2[!is.na(x2$base_date), ]
  return (x2)
}



# A1 Functions -----------------------------------------------------------

ptPerHHID <- function(x, start.date, end.date) {
  
  # Set phone dist.date for HH that moved to be the original phone.dist date
  x$phone.dist <- min(x$phone.dist)
  
  # If there are no monthly visits, pt will be from phone dist. to withdraw
  # (occurs when HH gets phone but withdraws before 1st monthly visit)
  if(nrow(x) == 1 && is.na(x$date_visit[1])) {
    x$pt[1] <- x$ppl_all[1] * as.numeric(x$with_date[1] - x$phone.dist[1])
  }
  
  # If there are no monthly visits because all visits occured before start date,
  # then HH dropped out after start date, but before next monthly visit pt will
  # be from phone dist. to withdraw
  else if (x$date_visit[1] < start.date) {
    for (i in 1:nrow(x)){
      if (x$date_visit[i] < start.date){
        x$date_visit[i] <- start.date
      }
    }
    # Keep only 1 of the visits that occured before start date - pt time will be
    # calculated from this
    #browser()
    x <- x[!duplicated(x$date_visit), ]
    x_rows <- nrow(x)
    
    # if the results if only 1 record, pt = time from "visit" until withdraw
    if (x_rows == 1){
      x$pt <-  x$ppl_all * as.numeric((x$with_date - x$phone.dist))
      
    } else if (x_rows > 2) {
      # Else if there are more than one row:
      # Calculate directly the 1st and last row's "pt"
      x$pt[1] <- x$ppl_all[1] * as.numeric(x$date_visit[2] - x$date_visit[1])
      x$pt[x_rows] <- x$ppl_all[x_rows] * as.numeric(x$with_date[x_rows] - x$date_visit[x_rows])
      # For middle rows, loop through and calculate "pt"
      for (i in 2:(x_rows- 1)){
        x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i +1] - x$date_visit[i])
      }
      
    } else if (x_rows == 2) {
      # Else if there are exactly 2 rows, a slightly differnt logic is required:
      x$pt[1] <- x$ppl_all[1] * as.numeric(x$date_visit[2] - x$date_visit[1])
      x$pt[2] <- x$ppl_all[2] * as.numeric(x$with_date[2] - x$date_visit[2])
    }
  }
  # If the first record is NA, but there are subsequent monthly visits:
  # (this occurs when HH gets phone, but moves locations before having monthly visit)
  else if (nrow(x) != 1 && is.na(x$date_visit[1])){
    # Remove visit dates occuring after end.date
    x1 <- x[1, ]
    x_remaining <- x[2:nrow(x), ]
    x_remaining <- x_remaining[x_remaining$date_visit <= end.date, ]
    x <- rbind(x1, x_remaining)
    rm(x1, x_remaining)
    for (i in 1:nrow(x)){
      if (i == 1){ # 1st record - make fake 'date_visit" equal to date of phone dist
        x$date_visit[i] <- x$phone.dist[i]
        x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - x$phone.dist[i])
      }
      else if (i > 1 && i < nrow(x)) {
        x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - (max(c(x$date_visit[i-1], x$phone.dist[i]))))
      } 
      else if (i > 1 && i == nrow(x)) {
        x$pt[i] <- x$ppl_all[i] *  as.numeric(x$date_visit[i] - max(c(x$date_visit[i-1], x$phone.dist[i]))) +
          x$ppl_all[i] * (x$with_date[i] - x$date_visit[i])
      }    
    }
  }
  
  # If Na is not in 1st record then there are data problems
  else if (nrow(x) != 1 && is.na(x$date_visit)){
    stop('Data error: date_visit is NA, but there are monthly visits
         recorded before the NA entry - check data!')
  }
  
  # Caluclates person-time since either last monthly visit or phone.dis - depending
  # on which was most recent.
  else {
    # If phone was distributed before end.date, 
    # but the 1st monthly-visit occurs after end.date then only use first row
    # of data and calculate pt between dist and withdraw
    if (x$phone.dist[1] <= end.date && x$date_visit[1] > end.date) {
      x <- x[1,]
      x$pt[1] <- x$ppl_all[1] * as.numeric(end.date - x$phone.dist[1])
    } 
    # subset to include only visits occuring before end.date and after start date
    else { 
      x <- x[x$date_visit <= end.date, ]
    }
    
    for (i in 1:nrow(x)) {
      if (i == 1 && x$date_visit[i] >= x$phone.dist[i] && nrow(x) > 1) {
        x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - x$phone.dist[i])
      } else if (i == 1 && x$date_visit[i] < x$phone.dist[i] && nrow(x) > 1) {
        x$pt[i] <- 0
      } else if (nrow(x) == 1) {
        x$pt[i] <- x$ppl_all[i] * (x$with_date[i] - x$phone.dist[i])
      } else if (is.na(x$date_visit[i])){
        x <- x[-i, ]
        warning("date_visit field is NA - probably problem with odk join")
      }
    }
    for (i in 1:nrow(x)){
      if (i > 1 && i < nrow(x)) {
        x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - (max(c(x$date_visit[i-1], x$phone.dist[i]))))
      } else if (i > 1 && i == nrow(x)) {
        x$pt[i] <- x$ppl_all[i] *  as.numeric(x$date_visit[i] - max(c(x$date_visit[i-1], x$phone.dist[i]))) +
          x$ppl_all[i] * (x$with_date[i] - x$date_visit[i])
      }
    }
  }
  for (i in 1:nrow(x)){
    if (is.na(x$uniqueID[i])){
      browser()
    }
  }
  
  return(x)
}





ptCalc <- function(x, start.date, end.date) {
  # Calculates person time over entire dataset. Uses 'ptPerHHID' function
  x <- x[x$phone.dist <= end.date, ]
  x$with_date[x$with_date > end.date] <- end.date 
  x$pt <- NA
  x1 <- x[complete.cases(x$uniqueID), ]
  x1 <- split(x1, f = x1$uniqueID)
  z <- lapply(x1, ptPerHHID, start.date = start.date, end.date = end.date)
  z <- do.call(rbind.data.frame, z)
  row.names(z) <- NULL
  z
}




pt48hr <- function(x, start.date, end.date) {
  # Calculates person time over entire dataset. Uses 'ptPerHHID' function
  #browser()
  x <- x[(x$phone.dist <= end.date & x$date_visit <= end.date), ]
  x <- x[(x$date_visit >= start.date), ]
  x$with_date[x$with_date > end.date] <- end.date 
  x1 <- x[complete.cases(x[, 1:7]), ]
  x1$pt48hr <- 2 * x1$ppl_all
  x1
}

