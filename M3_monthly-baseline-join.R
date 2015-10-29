# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files. Also adds HH_key to allow for tracking of
# HHs longitudinaly when they move compounds but remain in study.
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1, M2, & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       baseline.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       monthly_joined_path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata",
       monthly_joined_path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-odk-x2-joined.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")
# LOAD FILES --------------------------------------------------------------

# Monthly data
load(monthly_joined_path)

# Baseline data
load(baseline.path)

# load functions
source(functions.path)

# 1.) MERGE Baseline and monthly: KEEP all records ------------------------------------------

# Merge monthly visits with baseline. This drops any baseline records that don't
# have a monthly visit. 
z <- merge(visits.month, base_merge, by.x = "HHID", by.y = "HHID", all = T)


# Order rows and columns for easy reading ---------------------------------

# Rows
z <- z[order(z$HHID, z$base_date.x, z$date_visit), ]

# Columns
y <- match(c('uniqueID'), names(z))
x <- 1:ncol(z)
x <- x[-c(y)]
#z <- z[, c(y, x)]
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

# There are still two columns for listing number, I'm not sure which column is correct
m4$base_date.x <- NULL


# BASELINES WITHOUT MONTHLY VISITS ----------------------------------------

# Find all baselines that do not have a monthly visit
base_without_monthly_index <- !(base_merge$uniqueID %in% m4$uniqueID)
base_without_monthly <- base_merge[base_without_monthly_index, ]

# Make sure the number of columns and the order of colums is the same to allow
# rbind() to take place.
target.df <- data.frame(matrix(nrow = nrow(base_without_monthly), ncol = ncol(m4)))
names(target.df) <- names(m4)
names.vect <- match(names(target.df), names(base_without_monthly))

for (i in 1:length(names.vect)){
  if(!is.na(names.vect[i])){
  target.df[, i] <- base_without_monthly[, names.vect[i]]
  }
}

target.df$base_date <- as.Date(target.df$base_date, origin = "1970-01-01")
target.df$date_visit <- as.Date(target.df$date_visit, origin = "1970-01-01")

m4.1 <- rbind.data.frame(target.df, m4 )


y <- match(names(target), names(z1))
z2 <-z1[,]



# SAVE DATA ---------------------------------------------------------------
save(m4, file = data.output.path)

