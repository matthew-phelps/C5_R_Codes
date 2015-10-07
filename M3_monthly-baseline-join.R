# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, M2, & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       baseline.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       monthly_joined_path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata",
       monthly_joined_path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")
# LOAD FILES --------------------------------------------------------------

# Monthly data
load(monthly_joined_path)

# Baseline data
load(baseline.path)

# load functions
source(functions.path)

# 1.) MERGE Baseline and monthly: KEEP all records ------------------------------------------

z <- merge(visits.month, base_merge, by.x = "HHID", by.y = "HHID", all = T)



# Order rows and columns for easy reading ---------------------------------

# Rows
z <- z[order(z$HHID, z$base_date.x, z$date_visit), ]

# Columns
y <- match(c('uniqueID'), names(z))
x <- 1:ncol(base_merge)
x <- x[-c(y)]
rm(x, y)




# RECORD & REMOVE RECORDS WITHOUT BASELINE --------------------------------
# these should be fixed in B2 file

noBaseline <- z[is.na(z$base_date), ] 

z <- z[!is.na(z$base_date), ]
row.names(z) <- NULL


  
system.time({m3 <- hhCleanup(data = z, dateVisit = "date_visit", baseDate = "base_date.x",
          withdrawDate = "with_date", phoneDate = "phone.dist")})



# FORMATTING --------------------------------------------------------------

m3 <- m3[order(m3$HHID, m3$date_visit), ]
y <- match(c('uniqueID', 'base_date', "phone.dist", 'with_date'), names(m3))
x <- 1:ncol(m3)
x <- x[-c(y)]
m4<- m3[, c(y, x)]
rm(x, y, m3)
# SAVE DATA ---------------------------------------------------------------
save(m4, file = data.output.path)

