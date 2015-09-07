# Author: Matthew Phelps
# Desc:   Check for problems in data that emerge once monthly and baseline
#         data are joined  
# output: Cleaned monthly visits with baseline info
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
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

early.visit <- m4[m4$date_visit < m4$phone.dist - 2, ]
late.visit <- m4[m4$date_visit > m4$with_date, ]



# CLEAN DATA --------------------------------------------------------------




# RE-CHECK CLEAN DATA -----------------------------------------------------



# SAVE --------------------------------------------------------------------

save(m4, file = data.output.path)
