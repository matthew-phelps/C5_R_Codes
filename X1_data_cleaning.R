# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------


# Prepare Matthew's workspace if user == MATTHEW. If else, setwd to Chars dir
rm(list = ls())
graphics.off()
ifelse(grepl("zrc340", getwd()), 
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\baselineAll.Rdata",
       data.output.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baselineAll.Rdata")

#set working directory short cuts so they can be pasted together
ifelse(grepl("zrc340", getwd()), 
       wdmain <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data",
       wdmain <- "C:\\Users\\wrz741\\Dropbox")

wdx1 <- "\\C5 Field Operations data\\X-1 Cholera phone distribution"
setwd(paste(wdmain,wdx1,sep=""))

rm(wdx1,wdmain)



x1 <- read.csv("X-1 Choleraphone distribution 31Jul15.csv", sep=";")


# remove rows without a HHID
x1 <- x1[complete.cases(x1$HHID),]


# Format HHID and Listing number to have standard length. Then create unique ID
x1$HHID <-formatC(x1$HHID, width = 3, format = 'd', flag = 0)
x1$Listing.number <- formatC(x1$Listing.number, width = 4, format = 'd', flag = 0)
# turn date characters into date format
x1_2 <- lapply(x1[,3:5], as.Date, format = "%d.%m.%y")
x1 <- cbind(x1[c(1:2)], x1_2[1], x1_2[2], x1_2[3])
rm(x1_2)



# create unique ID called "unique_HHID_list_no"
x1$hh_listing_id <- paste(x1$HHID, x1$Listing.number, sep="-")
x1$HH_baseline <- paste(x1$HHID, x1$Date.of.baseline, sep="_")

# Number of times HHID & Listing number were used for separate baselines
length(unique(x1$HH_baseline)) - length(unique(x1$hh_listing_id))




# Save Data ---------------------------------------------------------------

save(x1, file = "C:/Users/wrz741/Dropbox/C5_R_Codes/Rdata/X-1 Choleraphone distribution 31Jul15.Rdata")
