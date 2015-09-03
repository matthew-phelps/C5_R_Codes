# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, M2, & B1, B2 to have been run


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
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "CHAR - PUT PATH TO CLEANED Monthly-Joined (M2 Output) HERE",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-baseline_join.Rdata")

# LOAD FILES --------------------------------------------------------------

# Monthly data
load(monthly_joined_path)

# Baseline data
load(baseline.path)


# 1.) MERGE and KEEP all records ------------------------------------------

z <- merge(visits.month, base_merge, by.x = "HHID", by.y = "HHID", all = T)



# Order rows and columns for easy reading ---------------------------------

# Rows
z <- z[order(z$HHID, z$base_date, z$date_visit), ]

# Columns
y <- match(c('uniqueID'), names(z))
x <- 1:(ncol(z) - length(y))
z <- z[, c(y, x)]
rm(x, y)




# RECORD & REMOVE RECORDS WITHOUT BASELINE --------------------------------
# these should be fixed upstream.
noBaseline <- z[is.na(z$base_date), ] 
z <- z[!is.na(z$base_date), ]
row.names(z) <- NULL


hhCleanup <- function(data, dateVisit, baseDate, withdrawDate, phoneDate) {
  # separates records that have same HHID but different baselines.
  x2 <- data.frame(t(c(1:ncol(data))))
  names(x2) <- names(data)
  for(i in 1:nrow(data)) {
    if (data[i, dateVisit] >= data[i, baseDate] &
        data[i, dateVisit] <= data[i, withdrawDate] ) {
      x2[i,] <- data[i,]
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

m3 <- hhCleanup(data = z, dateVisit = "date_visit", baseDate = "base_date",
          withdrawDate = "with_date", phoneDate = "phone.dist")



# SAVE DATA ---------------------------------------------------------------
save(m3, file = data.output.path)

