# Authors: Matthew Phelps
# DESC: Source all files to make running changes upstream easier




# Baseline Files ----------------------------------------------------------
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('B1 Joint baseline file.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('B2 Baseline cleaning.R')



# Monthly files -----------------------------------------------------------
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M1_ODK_cleaning.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M2_ODK_x2_join.R')

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M3_monthly-baseline-join.R')

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M4_cleaning-monthly-baseline.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M5_PersonTime_cleaning.R')

