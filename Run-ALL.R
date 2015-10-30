# Authors: Matthew Phelps
# DESC: Source all files to make running changes upstream easier




# Baseline Files ----------------------------------------------------------
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('B1 Joint baseline file.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('B2 Baseline cleaning.R')



# Monthly files -----------------------------------------------------------
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M1_ODK_cleaning.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M2_ODK_x2_join.R')

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M3_monthly-baseline-join.R')

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M4_cleaning-monthly-baseline2.R')


rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       wd.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes",
       wd.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes")

setwd(wd.path)
source('M5_PersonTime_cleaning.R')



# INSTALLED PACKAGES ------------------------------------------------------
# Creates .csv that lists all user-installed R packages that are currently
# installed. This is so we can easily re-create the R environment should we be on a
# new machine

rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       packages.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\list_of_r_packages.csv",
       packages.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\list_of_r_packages.csv")

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
if (nrow(ip) > 3){ # Don't overwrite when we first get to a new enviro
  write.csv(ip[,1], file = packages.path, row.names = F)
}
