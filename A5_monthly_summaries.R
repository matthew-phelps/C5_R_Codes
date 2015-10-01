# Author: Matthew Phelps
# Desc:    Monthly summaries. Record for each month:
#   * how many actiev HHs
#   * how many new phones distributed
#   * number of active participants
#   * number of monthly visits undertaken
# output: summary data
# DEPENDENCIES: Requires M1-M5 & B1, B2 to have been run


# Intro -------------------------------------------------------------------
library(dplyr)
library(data.table)

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())

ifelse(grepl("zrc340", getwd()),
       pt <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\person-time.Rdata",
       pt <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")


#detach("package:plyr", unload=TRUE) # disrupts the dplyr package