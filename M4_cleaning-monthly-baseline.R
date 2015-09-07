# Author: Matthew Phelps
# Desc:   Check for problems in data that emerge once monthly and baseline
#         data are joined  
# output: Cleaned monthly visits with baseline info
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()) + NA)
ifelse(grepl("zrc340", getwd()),
       monthly_basebase.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       monthly_basebase.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\clean-monthly-baseline.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")

# LOAD FILES --------------------------------------------------------------

load(monthly_basebase.path)
rm(monthly_basebase.path)




# INDEX PROBLEM RECORDS ---------------------------------------------------

early.phone <- m3[m3$date_visit < m3$phone.dist - 2, ]
