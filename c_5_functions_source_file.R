# Author: Matthew Phelps
# Desc:    Functions for C-5 project
# output: Function objects
# DEPENDENCIES: none

ptPerHHID <- function(x) {
  # Caluclates person-time since either last monthly visit or phone.dis - depending
  # on which was most recent.
  for (i in 1:nrow(x)) {
    if (i == 1 && x$date_visit[i] >= x$phone.dist[i] && nrow(x) > 1) {
      x$pt[i] <- x$ppl[i] * (x$date_visit[i] - x$phone.dist[i])
    } else if (i == 1 && x$date_visit[i] < x$phone.dist[i] && nrow(x) > 1) {
      x$pt[i] <- 0
    } else if (nrow(x) == 1) {
      x$pt[i] <- x$ppl[i] * (x$with_date[i] - x$phone.dist[i])
    } else if (i > 1 && i < nrow(x)) {
      x$pt[i] <- x$ppl[i] * as.numeric(x$date_visit[i] - (max(c(x$date_visit[i-1], x$phone.dist[i]))))
    } else if (i > 1 && i == nrow(x)) {
      x$pt[i] <- x$ppl[i] *  as.numeric(x$date_visit[i] - max(c(x$date_visit[i-1], x$phone.dist[i]))) +
        x$ppl[i] * (x$with_date[i] - x$date_visit[i])
    }
  } 
  return(x)
}
ptCalc <- function(x) {
  # Calculates person time over entire dataset. Uses 'ptPerHHID' function
  x$pt <- NA
  x1 <- x[complete.cases(x[, 1:7]), ]
  x1 <- split(x1, f = x1$uniqueID) 
  z <- lapply(x1, ptPerHHID)
  z <- do.call(rbind.data.frame, z)
  row.names(z) <- NULL
  z
}
