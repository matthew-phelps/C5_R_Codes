# Author: Matthew Phelps and Char Tamason
# Desc:    Merge ODK and X-2 files
# output: Cleaned monthly visits
# DEPENDENCIES: Requires M1 to have been run
 

# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
ifelse(grepl("zrc340", getwd()),
       NA,
       rm(list = ls()))
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
