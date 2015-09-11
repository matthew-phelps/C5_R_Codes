# Author: Char Tamason & Matthew Phelps
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1 to have been run


# Intro -------------------------------------------------------------------
library(dplyr)
library(xlsx)

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       x2.path <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Field Operations data/X-2 Monthly visit tracking sheet",
       x2.path <-"C:\\Users\\wrz741\\Dropbox\\C5 Field Operations data\\X-2 Monthly visit tracking sheet")
ifelse(grepl("zrc340", getwd()),
       odk <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\month_all.Rdata",
       odk <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")
ifelse(grepl("zrc340", getwd()),
       not.in.odk.csv.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\missing_from_ODK.csv",
       not.in.odk.csv.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\missing_from_ODK.csv")
ifelse(grepl("zrc340", getwd()),
       not.in.X2.csv.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\missing_from_X-2.csv",
       not.in.X2.csv.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\missing_from_X-2.csv")

# 1.) LOAD FILES ----------------------------------------------------------

# Functions:
source(functions.path)
 
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



# RESTRICT DATE RANGE -------------------------------------------------

date_wrong <- x2[x2$date_visit > end.date.x2 | x2$date_visit < start.date, ]
ifelse(nrow(date_wrong)==0, rm(date_wrong) + NA, NA)
rm(date_wrong)

x2 <- x2[x2$date_visit < odk.end.date, ]



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
# ODK dates
temp.x2.odk.merge <- merge(x2, MonthlyAll, by.x = c('HHID', 'date_visit'),  by.y = c('hh_id', 'visitdate') ,  all = T)

x <- split(temp.x2.odk.merge, temp.x2.odk.merge$HHID)

# Apply myFun1 to each element in the list formed by the "split" above:
system.time({x <- lapply(x, dateReplace)})

# Combine all data from duplicate rows into one row, then delete superfluous rows
system.time({x <- lapply(x, mergeRows)})

# Convert back to df
temp2 <- do.call(rbind.data.frame, x)

# Delete duplicates
z <- duplicated(temp2[, 1:3])
sum(z)
temp2 <- temp2[z == F,  ]
rm(temp.x2.odk.merge, z, x)


# Check missing records ---------------------------------------------------

not.in.x2 <- temp2[is.na(temp2$Listing.number), c("HHID", 'date_visit', 'FRA', 'Listing.number')]
not.in.x2 <- not.in.x2[order(not.in.x2$HHID, not.in.x2$date_visit), ]
row.names(not.in.x2) <- NULL

not.in.odk <- temp2[is.na(temp2$FRA), c("HHID", 'date_visit', 'ppl', "Listing.number")]
not.in.odk <- not.in.odk[order(not.in.odk$HHID, not.in.odk$date_visit), ]
row.names(not.in.odk) <- NULL




#  CLEAN based on logical assumptions---------------------------------------
# If dates are off by exactly 1 month, use ODK dates
#  If dates are off by a couple of day (+/- 3 days), use ODK dates

# Changes to ODK
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-26' & MonthlyAll$hh_id == 9] <- '2014-12-26'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-20' & MonthlyAll$hh_id == 28] <- '2014-11-18'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2015-06-08' & MonthlyAll$hh_id == 24] <- '2015-07-08'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2015-06-08' & MonthlyAll$hh_id == 30] <- '2015-07-08'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2015-06-07' & MonthlyAll$hh_id == 357] <- '2015-07-07'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-19' & MonthlyAll$hh_id == 170] <- '2014-10-14'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-14' & MonthlyAll$hh_id == 249] <- '2014-10-24'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-11-04' & MonthlyAll$hh_id == 313] <- '2014-11-07'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-11-04' & MonthlyAll$hh_id == 326] <- '2014-11-07'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-11-04' & MonthlyAll$hh_id == 353] <- '2014-11-07'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-11-04' & MonthlyAll$hh_id == 102] <- '2014-11-07'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-16' & MonthlyAll$hh_id == 367] <- '2014-10-17'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-17' & MonthlyAll$hh_id == 119] <- '2014-11-17'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2015-05-09' & MonthlyAll$hh_id == 286] <- '2015-06-09'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-24' & MonthlyAll$hh_id == 240] <- '2014-10-28'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-24' & MonthlyAll$hh_id == 282] <- '2014-10-28'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-24' & MonthlyAll$hh_id == 336] <- '2014-10-28'
MonthlyAll$visitdate[MonthlyAll$visitdate == '2014-10-23' & MonthlyAll$hh_id == 35] <- '2014-10-27'

# Deletions to ODK
MonthlyAll <- MonthlyAll[!(MonthlyAll$visitdate == '2014-09-15' & MonthlyAll$hh_id == 156), ]


# Changes to X2
x2$date_visit[x2$date_visit == '2015-04-09' & x2$HHID == 1] <- '2015-03-09'
x2$date_visit[x2$date_visit == '2015-04-06' & x2$HHID == 21] <- '2015-04-03'
x2$date_visit[x2$date_visit == '2015-04-06' & x2$HHID == 22] <- '2015-04-03'
x2$date_visit[x2$date_visit == '2015-04-06' & x2$HHID == 40] <- '2015-04-03'
x2$date_visit[x2$date_visit == '2015-04-06' & x2$HHID == 63] <- '2015-04-03'
x2$date_visit[x2$date_visit == '2015-04-06' & x2$HHID == 77] <- '2015-04-03'
x2$date_visit[x2$date_visit == '2014-09-11' & x2$HHID == 232] <- '2014-09-15'
x2$date_visit[x2$date_visit == '2014-10-20' & x2$HHID == 367] <- '2014-10-17'
x2$date_visit[x2$date_visit == '2015-04-13' & x2$HHID == 240] <- '2015-04-10'

# Deletions to X2
x2$date_visit[x2$date_visit == '2014-09-15' & x2$HHID == 156]

# RE-CHECK DATA AFTER CLEANING --------------------------------------------

temp.x3.odk.merge <- merge(x2, MonthlyAll, by.x = c('HHID', 'date_visit'),  by.y = c('hh_id', 'visitdate') ,  all = T)
x <- split(temp.x3.odk.merge, temp.x3.odk.merge$HHID)

x <- lapply(x, dateReplace)
x <- lapply(x, mergeRows)
temp3 <- do.call(rbind.data.frame, x)


# Check duplicates
z <- duplicated(temp3[, 1:3])
sum(z)
temp3 <- temp3[z == F,  ]
rm(temp.x3.odk.merge, z, x)

# Check missing records
not.in.x2 <- temp3[is.na(temp3$Listing.number), c("HHID", 'date_visit', 'FRA', 'Listing.number')]
not.in.x2 <- not.in.x2[order(not.in.x2$HHID, not.in.x2$date_visit), ]
row.names(not.in.x2) <- NULL

not.in.odk <- temp3[is.na(temp3$FRA), c("HHID", 'date_visit', 'ppl', "Listing.number")]
not.in.odk <- not.in.odk[order(not.in.odk$HHID, not.in.odk$date_visit), ]
row.names(not.in.odk) <- NULL


# WRITE TO CSV FOR BANGLADESH ---------------------------------------------

write.csv(not.in.odk, file = not.in.odk.csv.path, row.names = F)
write.csv(not.in.x2, file = not.in.X2.csv.path, row.names = F)

#  SAVE MERGRED ODK-X2 FILE-----------------------------------------------------
save(visits.month, file = data.output.path)




