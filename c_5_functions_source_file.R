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
  # On each row, test to see if there is a ODK entry, and the entry before or after was only a X2 entry
  # If row i is odk and row i+1 or i-1 is only x2 - take the date from row i and apply it to row i+1 or i-1
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



# M5 Functions ------------------------------------------------------------
ptPerHHID <- function(x, end.date) {
  # First section calculates person-time for HH where the phone was distributed
  # before end.date, but the 1st monthly-visit occurs after end.date
  if (x$phone.dist[1] <=end.date && x$date_visit[1] > end.date) {
    x <- x[1,]
    x$pt[1] <- x$ppl_all[1] * as.numeric(end.date - x$phone.dist[1])
  } else {
    x <- x[x$date_visit <= end.date, ]
  }
  # Caluclates person-time since either last monthly visit or phone.dis - depending
  # on which was most recent.
  for (i in 1:nrow(x)) {
    if (i == 1 && x$date_visit[i] >= x$phone.dist[i] && nrow(x) > 1) {
      x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - x$phone.dist[i])
    } else if (i == 1 && x$date_visit[i] < x$phone.dist[i] && nrow(x) > 1) {
      x$pt[i] <- 0
    } else if (nrow(x) == 1) {
      x$pt[i] <- x$ppl_all[i] * (x$with_date[i] - x$phone.dist[i])
    } else if (i > 1 && i < nrow(x)) {
      x$pt[i] <- x$ppl_all[i] * as.numeric(x$date_visit[i] - (max(c(x$date_visit[i-1], x$phone.dist[i]))))
    } else if (i > 1 && i == nrow(x)) {
      x$pt[i] <- x$ppl_all[i] *  as.numeric(x$date_visit[i] - max(c(x$date_visit[i-1], x$phone.dist[i]))) +
        x$ppl_all[i] * (x$with_date[i] - x$date_visit[i])
    }
  } 
  return(x)
}


ptCalc <- function(x, end.date) {
  # Calculates person time over entire dataset. Uses 'ptPerHHID' function
  x <- x[x$phone.dist <= end.date, ]
  x$with_date[x$with_date > end.date] <- end.date 
  x$pt <- NA
  x1 <- x[complete.cases(x[, 1:7]), ]
  x1 <- split(x1, f = x1$uniqueID)
  z <- lapply(x1, ptPerHHID, end.date = end.date)
  z <- do.call(rbind.data.frame, z)
  row.names(z) <- NULL
  z
}




pt48hr <- function(x, end.date) {
  # Calculates person time over entire dataset. Uses 'ptPerHHID' function
  x <- x[(x$phone.dist <= end.date & x$date_visit <= end.date), ]
  x$with_date[x$with_date > end.date] <- end.date 
  x1 <- x[complete.cases(x[, 1:7]), ]
  x1$pt48hr <- 2 * x1$ppl_all
  x1
}

