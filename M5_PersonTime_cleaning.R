# Author: Matthew Phelps
# Desc:    Calculate pt and check for errors based on pt
# output: PT per monthly visit per unique ID
# DEPENDENCIES: Requires M1-M4 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       clean_monthly_basebase.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata",
       clean_monthly_basebase.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\person-time.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")


library(dplyr)
library(data.table)
#detach("package:plyr", unload=TRUE) # disrupts the dplyr package


# FUNCTIONS ---------------------------------------------------------------

source(functions.path)

# GLOBAL VARIABLES  & FUNCTIONS--------------------------------------------------------
# endDate <- as.Date('31-12-14', "%d-%m-%y")
endDate <- Sys.Date()


# LOAD DATA ---------------------------------------------------------------

load(clean_monthly_basebase.path)


# SUBSET VARIABLES --------------------------------------------------------
m4 <- m4[, c("uniqueID", 'HHID', 'date_visit', 'ppl', 'base_date', 'phone.dist', 'with_date' )]




# PERSONE TIME for each household during each time-frame
# PERSON TIME 48-hr visits
m48hr <- pt48hr(m4, end.date = endDate)
m5 <- ptCalc(m4, end.date = endDate)



# ERROR CHECKING ----------------------------------------------------------

# Check individual entries for errors.
min(m5$pt, na.rm=T)
max(m5$pt)
which.max(m5$pt)
boxplot(m5$pt)


# Check households aggregated for errors.
# summarize pt by uniqueID:
households <- m5 %>%
  group_by(uniqueID) %>%
  summarise(
    pt_hh = sum(pt, na.rm = T)
  )

# Check outliers:
boxplot(households$pt_hh)




# SUM ---------------------------------------------------------------------

pt.days.phone <- sum(m5$pt)
pt.years.phone <- pt.days.phone / 365

pt.days.48hr <- sum(m48hr$pt48hr)
pt.years.48hr <- pt.days.48hr / 365

# WRITE TO FILE -----------------------------------------------------------

save(m5, file = data.output.path)
