# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()) + NA)
ifelse(grepl("zrc340", getwd()),
       baseline.path <- "CHAR - PUT PATH TO /Rdata HERE",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       monthly_joined_path <- "CHAR - PUT PATH TO CLEANED Monthly-Joined (M2 Output) HERE",
       monthly_joined_path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata")


# LOAD FILES --------------------------------------------------------------

# Monthly data
load(monthly_joined_path)

# Baseline data
load(baseline.path)


# temp files to work from
mz <- visits.month[, c(1:4)]
bz <- base_merge[, c(1:3, 7:9)]



# 1.) MERGE and KEEP all records ------------------------------------------

xz <- merge(mz, bz, by.x = "HHID", by.y = "HHID", all = T)

xz <- xz[order(xz$HHID, xz$base_date, xz$date_visit), ]
x <- xz[!is.na(xz$base_date), ]
row.names(x) <- NULL


hhCleanup <- function(x, dateVisit, baseDate, withdrawDate, phoneDate) {
  # separates HHs that moved within the same compound so had two baselines but same HHID and same listing No.
  x2 <- data.frame(t(c(1:ncol(x))))
  names(x2) <- names(x)
  for(i in 1:nrow(x)) {
    if (x[i, dateVisit] >= x[i, baseDate] &
        x[i, dateVisit] <= x[i, withdrawDate] ) {
      x2[i,] <- x[i,]
    } else {
      x2[i,] <- NA
    }
  }
  x2$date_visit <- as.Date(x2$date_visit, origin = "1970-01-01")
  x2$base_date <- as.Date(x2$base_date, origin = "1970-01-01")
  x2$phone.dist <- as.Date(x2$phone.dist, origin = "1970-01-01")
  x2$with_date <- as.Date(x2$with_date, origin = "1970-01-01")
  x2 <- x2[!is.na(x2$base_date), ]
  return (x2)
}


x3 <- hhCleanup(x, dateVisit = "date_visit", baseDate = "base_date",
          withdrawDate = "with_date", phoneDate = "phone.dist")



# # Restrict to last X1 date --=- MOVE TO LATER IN CODE
# visits.end.date <- max(visits.month$date_visit)
# base.end.date <- max(base_merge$with_date)
# ifelse(base.end.date <= visits.end.date,
#        visits.month <- visits.month[visits.month$date_visit <= base.end.date, ],
#        base_merge <- bbase_merge[base_merge$base_date <= visits.end.date, ])                   
# 
# 
