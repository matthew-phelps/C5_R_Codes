# Authors: Char Tamason and Matthew Phelps
# Desc: Combine X1 and data-entry Baseline, then cleans the baseline data
# Output: Clean, joined, baseline data
# DEPENDENCIES: must run B1 and M1 first


# INTRO -------------------------------------------------------------------


# If user == Char, do nothing. If else, prepare Matthew's workingspace
rm(list = ls())
ifelse(grepl("zrc340", getwd()), 
       B1.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\baselineAll.Rdata",
       B1.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baselineAll.Rdata")

ifelse(grepl("zrc340", getwd()), 
       B2.base.merge.output.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata",
       B2.base.merge.output.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()), 
       not.in.x1.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\Missing_from_X1.csv",
       not.in.x1.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Missing_from_X1.csv")
ifelse(grepl("zrc340", getwd()), 
       not.in.baseline.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\Missing_from_baseline.csv",
       not.in.baseline.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Missing_from_baseline.csv")
ifelse(grepl("zrc340", getwd()), 
       overlap.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\Overlap.csv",
       overlap.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Overlap.csv")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")

ifelse(grepl("zrc340", getwd()), 
       wdmain <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data",
       wdmain <- "C:\\Users\\wrz741\\Dropbox")


wdx1<-"\\C5 Field Operations data\\X-1 Cholera phone distribution"
setwd(paste(wdmain,wdx1,sep=""))

library(xlsx)



# LOAD DATA ---------------------------------------------------------------

load(B1.path)
source(functions.path)

# GLOBAL VARIABLES --------------------------------------------------------
cut.date <- as.Date("2015-05-20")


# LOAD DATA ---------------------------------------------------------------


# 1.) Load most recent X-1 version:

fileNames.df <- file.info(list.files(path = getwd(),
                                     pattern = "X-1 Choleraphone distribution.*\\.xlsx$", 
                                     full.names = T))

# Sort files that match pattern by most modification time and 
# extract the rowname of the most recently modified file:
fileNames.df <- fileNames.df[with(fileNames.df, order(as.POSIXct(mtime))), ]
x1.name <- fileNames.df[nrow(fileNames.df), ] # Subset the most recently modified file
x1.path <- rownames(x1.name) # Get file path
x1_data <- read.xlsx2(file = x1.path, sheetIndex = 1, stringsAsFactors = F) # turn into df

# Store end.date for data validation (make sure no records are after the file creation date)
end.date <- as.Date(x1.name$mtime)
rm(x1.name, x1.path, fileNames.df)

# Formate dates of x1
x1_data$base_date <- as.Date(x1_data$Date.of.baseline, "%d.%m.%y")
x1_data$with_date <- as.Date(x1_data$Date.of.withdrawl.or.move, "%d.%m.%y")
x1_data$phone.dist <- as.Date(x1_data$Date.of.phone.distribution, "%d.%m.%y")

dropvars <- c("Date.of.phone.distribution", "Date.of.withdrawl.or.move")
x1_data <- x1_data[, !names(x1_data) %in% dropvars]

# Only keep records with a HHID from range of dates for which baseline has been entered
x1_data$HHID <- as.numeric(x1_data$HHID)
x1_data <- x1_data[!is.na(x1_data$HHID), ]
x1_data <- x1_data[x1_data$base_date <= cut.date, ]

# Will change phone dist date for HHS that move in order to calculate pt correctly
# So need new variable to track originally recorded phone.dist date
x1_data$phone.dist.original <- x1_data$phone.dist




# Create unique IDs for x1
x1_data$HHID <- formatC(x1_data$HHID, width = 3, format = 'd', flag = 0)
x1_data$HHID_character <- formatC(x1_data$HHID, width = 3, format = 'd', flag = 0)
x1_data$uniqueID <- paste(x1_data$HHID, "_", x1_data$base_date, sep="")

# Return HHID to numeric format
x1_data$HHID <- as.numeric(x1_data$HHID)

# Set current_date to day data was downloaded
current_date <- end.date

# Any records missing a withdrawl date are assigned the current date as withdrawl date
x1_data$with_date[is.na(x1_data$with_date)] <- current_date


x1_data$interval_check <- NA


# 2.) Check data: which unique IDs in x1 are not in baseline. Create df of x1 not in baseline --------------------------------------------------------------------

not.in.baseline <- (x1_data[!(x1_data$uniqueID %in% baselineAll$uniqueID), ])

# 3.) Perform Cleaning ----------------------------------------------------

# Change dates in X-1 that are off by a keystroke to match baseline
x1_data$base_date[x1_data$uniqueID== "083_2014-09-12"] <- "2014-09-11"
x1_data$base_date[x1_data$uniqueID== "100_2014-11-25"] <- "2014-11-24"
x1_data$base_date[x1_data$uniqueID== "171_2014-10-14"] <- "2014-11-14"
x1_data$base_date[x1_data$uniqueID== "230_2014-08-15"] <- "2014-08-16"
x1_data$base_date[x1_data$uniqueID== "235_2014-09-16"] <- "2014-09-15"
x1_data$base_date[x1_data$uniqueID== "248_2014-08-07"] <- "2014-07-07"
x1_data$base_date[x1_data$uniqueID== "028_2014-09-12"] <- "2014-09-11"
x1_data$base_date[x1_data$uniqueID== "295_2014-08-18"] <- "2014-08-08"

x1_data$base_date[x1_data$uniqueID== "028_2014-09-12"] <- "2014-09-11"
x1_data$base_date[x1_data$uniqueID== "083_2014-09-12"] <- "2014-09-11"

# # Changes to X-1 HHIDs based on email from Bimal 2015-09-08:
x1_data$base_date[x1_data$uniqueID== "220_2014-06-11"] <- "2014-07-11"
x1_data$base_date[x1_data$uniqueID== "253_2014-08-26"] <- "2014-08-15"

# # Changes to baseline HHID based on email from Bimal:
baselineAll$hhid[baselineAll$uniqueID == "172_2014-10-21"] <- 60

baselineAll$hhid[baselineAll$uniqueID == "114_2014-10-20"] <- 144
baselineAll$hhid[baselineAll$uniqueID == "168_2014-08-22"] <- 184

baselineAll$hhid[baselineAll$uniqueID == "151_2014-08-19"] <- 251
baselineAll$hhid[baselineAll$uniqueID == "124_2014-11-14"] <- 300
baselineAll$hhid[baselineAll$uniqueID == "191_2014-10-27"] <- 322
baselineAll$hhid[baselineAll$uniqueID == "182_2014-11-14"] <- 332
baselineAll$hhid[baselineAll$uniqueID == "236_2014-11-14"] <- 391

# Changes to baseline HHIDs based on email from Bimal 2015-09-08:
baselineAll$hhid[baselineAll$uniqueID =='117_2014-10-24'] <- 171
baselineAll$hhid[baselineAll$uniqueID =='382_2014-07-11'] <- 220
baselineAll$hhid[baselineAll$uniqueID =='054_2014-09-25'] <- 254
baselineAll$hhid[baselineAll$uniqueID == '088_2014-06-01'] <- 85
baselineAll$hhid[baselineAll$uniqueID == '155_2014-10-31'] <- 217

# # Changes to baseline dates on email from Bimal:
baselineAll$base_date[baselineAll$uniqueID == "333_2014-04-20"] <- "2014-07-20"
baselineAll$base_date [baselineAll$uniqueID == "067_2014-12-09"] <- "2014-06-05"
baselineAll$base_date [baselineAll$uniqueID == "252_2014-12-12"] <- "2014-06-01"

# Re-create unique IDs with new dates
baselineAll$hhid <- formatC(baselineAll$hhid, width = 3, format = 'd', flag = 0)
baselineAll$uniqueID<-paste(baselineAll$hhid,"_",baselineAll$base_date,sep="")
baselineAll$hhid <- as.numeric(baselineAll$hhid)

x1_data$HHID <- formatC(x1_data$HHID, width = 3, format = 'd', flag = 0)
x1_data$uniqueID<-paste(x1_data$HHID,"_",x1_data$base_date,sep="")
x1_data$HHID <- as.numeric(x1_data$HHID)


# DROPOUTS FROM BASELINE ----------------------------------------------------

# People to record as dropouts between baseline and monthly
dropouts <-data.frame(uniqueID = c("300_2014-08-22",
                                   "322_2014-07-12",
                                   "332_2014-08-14",
                                   "391_2014-07-11",
                                   '018_2014-09-19',
                                   '027_2014-08-27',
                                   '047_2014-07-20',
                                   '049_2014-09-12',
                                   '050_2014-09-05',
                                   '075_2014-09-15',
                                   '123_2014-08-06',
                                   '145_2014-10-24',
                                   '184_2014-08-22',
                                   '171_2014-11-14',
                                   '186_2014-08-17',
                                   '189_2014-10-13',
                                   '205_2014-10-31',
                                   '217_2014-07-25',
                                   '222_2014-08-22',
                                   '229_2014-10-17',
                                   '245_2014-08-19',
                                   '237_2014-12-03',
                                   '254_2014-08-08',
                                   '274_2014-08-04',
                                   '287_2014-08-09',
                                   '293_2014-08-08',
                                   '323_2014-08-25',
                                   '330_2014-08-13',
                                   '346_2014-07-16',
                                   '351_2014-07-16',
                                   '357_2014-07-14',
                                   '357_2014-12-02',
                                   '393_2014-07-16'
                                   ))
# Not dropouts, just errors
omitt <- data.frame(uniqueID = c("253_2014-08-15"))
                    
# x1_data$uniqueID[x1_data$uniqueID == "184_201-11-24"] == baselineAll$uniqueID[baselineAll$uniqueID == "155_2014-10-31"]
# baselineAll$uniqueID[baselineAll$uniqueID == "155_2014-10-31"]

# Remove records from baseline:
baselineAll <- baselineAll[!(baselineAll$uniqueID %in% dropouts$uniqueID),]
baselineAll <- baselineAll[!(baselineAll$uniqueID %in% omitt$uniqueID),]






# 3.) Repeat data check after cleaning -------------------------------
not.in.baseline.2 <- (x1_data[!(x1_data$uniqueID %in% baselineAll$uniqueID), ])
rm(not.in.baseline)
x1_data$uniqueID[x1_data$uniqueID == "382_2014-07-11"] == baselineAll$uniqueID[baselineAll$uniqueID == "382_2014-07-11"]
write.csv2(not.in.baseline.2[, c(8, 1:5)], 
           file =not.in.baseline.path,
           row.names = F)


# 4.) Check Data: Baseline records that are not in x-1 ------------------------

x1_data <- x1_data[order(x1_data$HHID), ]

not_in_X1<-as.data.frame(baselineAll[!(baselineAll$uniqueID %in% x1_data$uniqueID), c("uniqueID", "hhid", "base_date")])



# Checked for typos. None found.

# Remove HHIDs that Bimal already addressed:
not_in_X1 <- not_in_X1[!(not_in_X1$hhid == 67 | 
                           not_in_X1$hhid == 168 |
                           not_in_X1$hhid == 252), ]
 

# 5.) CLEANING  ---------------------------------------------------------

## To-be-done after getting info back from BD


# 6.) RE-RUN DATA CHECK AFTER CLEANING X1 -------------------------------------

not_in_X1<-as.data.frame(baselineAll[!(baselineAll$uniqueID %in% x1_data$uniqueID), c("uniqueID", "hhid", "listing", "base_date")])
names(not_in_X1)[4] <- "baseline_date"

# Send remaing records to Bangladesh for checking
write.csv2(not_in_X1, 
           file = not.in.x1.path,
           row.names = F)


#check which baseline entries are in x1, can be used for analysis
baseline_in_X1<-c(sort(baselineAll$uniqueID[(baselineAll$uniqueID %in% x1_data$uniqueID)]))





# 6.) Check date ranges -------------------------------------------------------
# (& make sure same HHIDs do not overlap in time) 
summary(x1_data$base_date)
summary(baselineAll$base_date)

baselineAll$uniqueID

# Interval Check: Frist, sort data set interval_check to be T for all records:
x1_data$interval_check <- T

# Group items together by hhid. List contains df with a record for each 
# hhid + baseline combination.
x1_split <- split(x1_data, x1_data$HHID)

# Loop through list, if more than one baseline given to a hh, check to make sure
# the dates are logical. i.e. baseline of j + 1 is after the withdrawl of j.
for (i in 1:length(x1_split)){
  if(nrow(x1_split[[i]]) > 1) {
    for (j in 1:(nrow(x1_split[[i]]) - 1)){ # If wthdrwl date > base of next hh, give it a F
      ifelse(x1_split[[i]]$with_date[j] >  x1_split[[i]]$base_date[j + 1],
             x1_split[[i]]$interval_check[j] <- F, x1_split[[i]]$interval_check[j] <- T )
    }
  } 

}

# Turn list back to df
x1 <- do.call(rbind.data.frame, x1_split)
row.names(x1) <- NULL
rm(x1_data, x1_split)

# There are no interval errors when this statement is TRUE: 
nrow(x1) == sum(x1$interval_check)

# Df of problem records:
y <- which(x1$interval_check %in% F) #Gives index of problem cells
e1 <- x1[c(y, y+1), ] # gives df of problem cells y-1 makes sure we get the
e1 <- e1[order(e1$HHID), ]

# Write csv to send to BD of problems
write.csv2(e1[, c(8, 1:5)],
           file = overlap.path,
           row.names = F)




# MOVING VS DROPOUTS ------------------------------------------------------
# Change phone.dist date for houses that moved but stayed in study.

x1 <- moveDates(x = x1, factor = x1$HHID)
row.names(x1) <- NULL



# HH KEY ------------------------------------------------------------------

# HH_key is given by "date of phone dist" + "HHID"
x1$phone.dist.original.nice <- format(x1$phone.dist.original, format = "%d-%b-%Y")
x1$HH_key <- paste(x1$phone.dist.original.nice, x1$HHID_character, sep="_")




# 8.) MERGE x1 and baseline --------------------------

base_merge <- merge(x1, baselineAll, by.x = "uniqueID", by.y = "uniqueID",
                    all = F)
base_merge$base_date.x == base_merge$base_date.y
dropvars <- c("hhid", "hhid.1", "base_date.y")
base_merge <- base_merge[, !names(base_merge) %in% dropvars]


# SAVE OUTPUT -------------------------------------------------------------


save(base_merge, file = B2.base.merge.output.path)





