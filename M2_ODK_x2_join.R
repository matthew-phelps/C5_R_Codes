# Author: Char Tamason & Matthew Phelps
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1 to have been run


# Intro -------------------------------------------------------------------
library(dplyr)
library(xlsx)

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()) + NA)
ifelse(grepl("zrc340", getwd()),
       x2.path <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Field Operations data/X-2 Monthly visit tracking sheet",
       x2.path <-"C:\\Users\\wrz741\\Dropbox\\C5 Field Operations data\\X-2 Monthly visit tracking sheet")
ifelse(grepl("zrc340", getwd()),
       odk <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\month_all.Rdata",
       odk <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata")



# 1.) LOAD FILES ----------------------------------------------------------

# ODK Cleaned data
load(odk)

# Most recent X2 data
fileNames.df <- file.info(list.files(path = x2.path,
                                     pattern = "X-2 monthly visits.*\\.xlsx$", full.names = T))
# Sort files that match pattern by most modification time and 
# extract the rowname of the most recently modified file:
fileNames.df <- fileNames.df[with(fileNames.df, order(as.POSIXct(mtime))), ]
x2.name <- fileNames.df[nrow(fileNames.df), ] # Subset the most recently modified file
x2.path <- rownames(x2.name) # Get file path
x2 <- read.xlsx2(file = x2.path, sheetIndex = 1, stringsAsFactors = F) # turn into df




# GLOBAL VARIABLES --------------------------------------------------------

# Store latest X-2 upload date from Bangladesh for data validation (make sure no records are after the file creation date)
end.date.x2 <- as.Date(x2.name$mtime)
rm(x2.name, x2.path, fileNames.df)
start.date <- as.Date("2014-09-09")

odk.end.date <- max(MonthlyAll$visitdate)


# 2.) Format data to make easy to work with ------------------------------------------------------

# x2 dates
x2$date_visit <- as.Date(x2$Date.of.monthly.visit, format = "%d.%m.%y")
x2 <- x2[order(x2$HHID, x2$date_visit), ]
row.names(x2) <- NULL

# make HHID inteter
x2$HHID <- as.integer(x2$HHID)

# Remove empyt records
x2 <- x2[!is.na(x2$HHID), ]

# clean columns for simplicity
x2$Date.of.monthly.visit <- NULL
x2$ppl <- as.numeric(x2$Numer.of.ppl.in.household.at.monthly.visit)
x2$Numer.of.ppl.in.household.at.monthly.visit <- NULL



# Check X2 for date range -------------------------------------------------

date_wrong <- x2[x2$date_visit > end.date.x2 | x2$date_visit < start.date, ]
rm(date_wrong)




# 3.) CHECK DUPLICATE RECORDS ---------------------------------------------

# Check ODK for where HH was recorded as being visited twice on same date:
x <- (duplicated(MonthlyAll[, c(1:11)]))
y <- which(x %in% T) #Gives index of duplicates
duplicates.odk <- MonthlyAll[c(y, y-1), ] # gives df of duplicates. y-1 makes sure we get the
# 'original' and the 'duplicate'

# Remove duplicates as needed based on info from BD:
# Drop HHID 185 duplicate
# Need to delete row 237 (?) or 109 (?) depending on word from Bangladesh
droprows <- y
MonthlyAll <- MonthlyAll[-c(droprows), ]
rm(x, y, duplicates.odk)

# Check X2 for where HH was recorded as being visited twice on same date:
x <- (duplicated(x2[, c('date_visit', 'HHID')]))
y <- which(x %in% T) #Gives index of duplicates
duplicates.x2 <- x2[c(y, y-1), ] # gives df of duplicates. y-1 makes sure we get the
# 'original' and the 'duplicate'
rm(x, y, duplicates.x2)




# Check for typos in entry ------------------------------------------------

# We assume that if there is a 1 - 2 day differene between ODK and X2, we use
# X2 dates CHANGE - NEED TO USE ODK
temp.merge <- merge(x2, MonthlyAll, by.x = c('HHID', 'date_visit'),  by.y = c('hh_id', 'visitdate') ,  all = T)

x <- split(temp.merge, temp.merge$HHID)

# Apply myFun1 to each element in the list formed by the "split" above:
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

system.time({x <- lapply(x, dateReplace)})

# Combine all data from duplicate rows into one row, then delete superfluous rows

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
system.time({x <- lapply(x, mergeRows)})

# Convert back to df
temp2 <- do.call(rbind.data.frame, x)

# Delete duplicates
z <- duplicated(temp2[, 1:3])
sum(z)
temp2 <- temp2[z == F,  ]
rm(temp.merge, z, x)


# Check missing records ---------------------------------------------------

not.in.x2 <- temp2[is.na(temp2$Listing.number), c("HHID", 'date_visit', 'FRA', 'Listing.number')]
not.in.x2 <- not.in.x2[order(not.in.x2$HHID, not.in.x2$date_visit), ]
row.names(not.in.x2) <- NULL

not.in.odk <- temp2[is.na(temp2$FRA), c("HHID", 'date_visit', 'ppl', "Listing.number")]
not.in.odk <- not.in.odk[order(not.in.odk$HHID, not.in.odk$date_visit), ]
row.names(not.in.odk) <- NULL




#  CLEAN based on Bangladesh response---------------------------------------



# RE-CHECK DATA AFTER CLEANING --------------------------------------------

# Check duplicates
z <- duplicated(temp2[, 1:3])
sum(z)

# Check missing records
not.in.x2 <- temp2[is.na(temp2$Listing.number), c("HHID", 'date_visit', 'FRA', 'Listing.number')]
not.in.x2 <- not.in.x2[order(not.in.x2$HHID, not.in.x2$date_visit), ]
row.names(not.in.x2) <- NULL

not.in.odk <- temp2[is.na(temp2$FRA), c("HHID", 'date_visit', 'ppl', "Listing.number")]
not.in.odk <- not.in.odk[order(not.in.odk$HHID, not.in.odk$date_visit), ]
row.names(not.in.odk) <- NULL

# Account for delay in ODK reporting
not.in.odk <- not.in.odk[not.in.odk$date_visit <= odk.end.date, ]

visits.month <- temp2[temp2$date_visit <= odk.end.date, ]


#  SAVE MERGRED ODK-X2 FILE-----------------------------------------------------
save(visits.month, file = data.output.path)





######## END - code below is not functional ################################
######## 
# 4.) COMBINE TO X-2 ------------------------------------------------------


