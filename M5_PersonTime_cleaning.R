# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       clean_monthly_basebase.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata",
       clean_monthly_basebase.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\person-time.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")

library(dplyr)
library(data.table)
#detach("package:plyr", unload=TRUE) # disrupts the dplyr package


# FUNCTIONS ---------------------------------------------------------------


# GLOBAL VARIABLES  & FUNCTIONS--------------------------------------------------------
endDate <- as.Date('31-12-14', "%d-%m-%y")
endDate <- Sys.Date()

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


# LOAD DATA ---------------------------------------------------------------

load(clean_monthly_basebase.path)


# SUBSET VARIABLES --------------------------------------------------------
m4 <- m4[, c("uniqueID", 'HHID', 'date_visit', 'ppl', 'base_date', 'phone.dist', 'with_date' )]



# RESTRICT DATE -----------------------------------------------------------

m4 <- m4[m4$date_visit <= endDate, ]

# Set withdraw data as end date for any records where true with draw is after end date
m4$with_date[m4$with_date > endDate] <- endDate

min(m4$with_date)
max(m4$with_date)

# PERSONE TIME for each household during each time-frame

m5 <- ptCalc(m4)



# ERROR CHECKING ----------------------------------------------------------

# Check individual entries for errors.
min(m5$pt, na.rm=T)
max(m5$pt)
which.max(m5$pt)
boxplot(m5$pt)


# Check households aggregated for errors.
# summarize pt by uniqueID:
households <- m5 %>%
  group_by(uniqueID) %>%
  summarise(
    pt_hh = sum(pt, na.rm = T)
  )

# Check outliers:
boxplot(households$pt_hh)

# ANALYSIS ----------------------------------------------------------------

# WRITE TO FILE -----------------------------------------------------------

save(m5, file = data.output.path)
