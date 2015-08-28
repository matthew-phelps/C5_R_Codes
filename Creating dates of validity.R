#Creating dates of validity for each uniqueID, using X-1 Choleraphone distribution sheet from field station
####must run creating a joint baseline file and Creating joint monthly visit file2 first
wdx1<-"\\C5 Field Operations data\\X-1 Cholera phone distribution"
setwd(paste(wdmain,wdx1,sep=""))

x1_data <- read.csv2("X-1 Choleraphone distribution 31Jul15.csv")
x1_data <- x1_data[which(!(is.na(x1_data$HHID))),]

x1_data$base_date <- as.Date(x1_data$Date.of.baseline, "%d.%m.%y")
x1_data$with_date <- as.Date(x1_data$Date.of.withdrawl.or.move, "%d.%m.%y")
x1_data$uniqueID <- paste(x1_data$HHID, "_", x1_data$base_date, sep="")

# set current_date to day data was downloaded
current_date <- "2015-08-15"
x1_data$with_date[is.na(x1_data$with_date)] <- current_date
x1_data$interval_check <- NA

#cleaning, see which uniqueids in x1 are not in baseline

sort(x1_data$uniqueID[!(x1_data$uniqueID %in% baselineAll$uniqueID)])

missing20_08_2015<-("100_2014-11-25", "109_2015-05-27", "118_2015-05-31", "126_2015-06-21", "129_2015-06-24", "144_2014-10-20", "16_2014-06-03", 
                    "168_2015-05-08", "171_2014-10-24", "186_2015-05-24", "199_2015-06-01", "214_2015-06-05", "217_2014-10-31", "220_2014-06-11",
                    "223_2015-04-17", "225_2015-06-28", "230_2014-08-15", "235_2014-09-16", "248_2014-08-07", "251_2014-08-19", "252_2014-06-01",
                    "253_2014-08-26", "254_2014-09-25", "257_2015-05-23", "270_2015-06-12", "28_2014-09-12",  "280_2015-06-26", "295_2014-08-18",
                    "300_2014-11-14", "322_2014-10-27", "330_2015-06-26", "332_2014-11-14", "333_2014-07-20", "35_2015-07-24",  "388_2015-07-11",
                    "391_2014-11-14", "50_2015-05-29",  "60_2014-10-21",  "67_2014-06-05",  "78_2015-07-04",  "83_2014-09-12",  "85_2014-06-01", 
                    "90_2015-07-04") 


baselineAll$uniqueID[baselineAll$hhid==50]
x1_data$uniqueID[x1_data$HHID==50]
baselineAll$uniqueID[baselineAll$hhid==60]
x1_data$uniqueID[x1_data$HHID==60]
baselineAll$uniqueID[baselineAll$hhid==67]
x1_data$uniqueID[x1_data$HHID==67]
baselineAll$uniqueID[baselineAll$hhid==78]
x1_data$uniqueID[x1_data$HHID==78]
baselineAll$uniqueID[baselineAll$hhid==83]
x1_data$uniqueID[x1_data$HHID==83]
baselineAll$uniqueID[baselineAll$hhid==85]
x1_data$uniqueID[x1_data$HHID==85]
baselineAll$uniqueID[baselineAll$hhid==90]
x1_data$uniqueID[x1_data$HHID==90]
baselineAll$uniqueID[baselineAll$hhid==100]
x1_data$uniqueID[x1_data$HHID==100]
baselineAll$uniqueID[baselineAll$hhid==109]
x1_data$uniqueID[x1_data$HHID==109]
baselineAll$uniqueID[baselineAll$hhid==118]
x1_data$uniqueID[x1_data$HHID==118]
baselineAll$uniqueID[baselineAll$hhid==126]
x1_data$uniqueID[x1_data$HHID==126]
baselineAll$uniqueID[baselineAll$hhid==129]
x1_data$uniqueID[x1_data$HHID==129]
baselineAll$uniqueID[baselineAll$hhid==144]
x1_data$uniqueID[x1_data$HHID==144]
baselineAll$uniqueID[baselineAll$hhid==16]
x1_data$uniqueID[x1_data$HHID==16]
baselineAll$uniqueID[baselineAll$hhid==168]
x1_data$uniqueID[x1_data$HHID==168]
baselineAll$uniqueID[baselineAll$hhid==171]
x1_data$uniqueID[x1_data$HHID==171]
baselineAll$uniqueID[baselineAll$hhid==186]
x1_data$uniqueID[x1_data$HHID==186]
baselineAll$uniqueID[baselineAll$hhid==199]
x1_data$uniqueID[x1_data$HHID==199]
baselineAll$uniqueID[baselineAll$hhid==214]
x1_data$uniqueID[x1_data$HHID==214]
baselineAll$uniqueID[baselineAll$hhid==217]
x1_data$uniqueID[x1_data$HHID==217]
baselineAll$uniqueID[baselineAll$hhid==220]
x1_data$uniqueID[x1_data$HHID==220]
baselineAll$uniqueID[baselineAll$hhid==223]
x1_data$uniqueID[x1_data$HHID==223]
baselineAll$uniqueID[baselineAll$hhid==225]
x1_data$uniqueID[x1_data$HHID==225]
baselineAll$uniqueID[baselineAll$hhid==230]
x1_data$uniqueID[x1_data$HHID==230]
baselineAll$uniqueID[baselineAll$hhid==235]
x1_data$uniqueID[x1_data$HHID==235]
baselineAll$uniqueID[baselineAll$hhid==248]
x1_data$uniqueID[x1_data$HHID==248]
baselineAll$uniqueID[baselineAll$hhid==251]
x1_data$uniqueID[x1_data$HHID==251]
#------------ trying to convert monthly visit autodates into separate variables to form actual date (too many misentries) #autodate didn't work
baselineAll$uniqueID[baselineAll$hhid==252] 
x1_data$uniqueID[x1_data$HHID==252]
baselineAll$uniqueID[baselineAll$hhid==253]
x1_data$uniqueID[x1_data$HHID==253]
baselineAll$uniqueID[baselineAll$hhid==254]
x1_data$uniqueID[x1_data$HHID==254]
baselineAll$uniqueID[baselineAll$hhid==257]
x1_data$uniqueID[x1_data$HHID==257]
baselineAll$uniqueID[baselineAll$hhid==270]
x1_data$uniqueID[x1_data$HHID==270]
baselineAll$uniqueID[baselineAll$hhid==28]
x1_data$uniqueID[x1_data$HHID==28]
baselineAll$uniqueID[baselineAll$hhid==280]
x1_data$uniqueID[x1_data$HHID==280]
baselineAll$uniqueID[baselineAll$hhid==295]
x1_data$uniqueID[x1_data$HHID==295]
baselineAll$uniqueID[baselineAll$hhid==300]
x1_data$uniqueID[x1_data$HHID==300]
baselineAll$uniqueID[baselineAll$hhid==322]
x1_data$uniqueID[x1_data$HHID==322]
baselineAll$uniqueID[baselineAll$hhid==330]
x1_data$uniqueID[x1_data$HHID==330]
#MonthlyAll$base_date[MonthlyAll$hh_id==330]
baselineAll$uniqueID[baselineAll$hhid==332]
x1_data$uniqueID[x1_data$HHID==332]
baselineAll$uniqueID[baselineAll$hhid==333]
x1_data$uniqueID[x1_data$HHID==333]
baselineAll$uniqueID[baselineAll$hhid==333]
x1_data$uniqueID[x1_data$HHID==333]
baselineAll$uniqueID[baselineAll$hhid==35]
x1_data$uniqueID[x1_data$HHID==35]
baselineAll$uniqueID[baselineAll$hhid==388]
x1_data$uniqueID[x1_data$HHID==388]
baselineAll$uniqueID[baselineAll$hhid==391]
x1_data$uniqueID[x1_data$HHID==391]

#change dates in X-1 that are off by a keystroke to match baseline
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="83_2014-09-12","83_2014-09-11",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="100_2014-11-25","100_2014-11-24",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="171_2014-10-14","171_2014-11-14",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="230_2014-08-15","230_2014-08-16",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="235_2014-09-16","235_2014-09-15",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="248_2014-08-07","248_2014-07-07",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="28_2014-09-12","28_2014-09-11",x1_data$uniqueID)
x1_data$uniqueID<-ifelse(x1_data$uniqueID=="295_2014-08-18","295_2014-08-08",x1_data$uniqueID)

#change dates in baseline that are incorrect, in order to match with X-1
baselineAll$uniqueID<-ifelse(baselineAll$uniqueID=="333_2014-04-20","333_2014-07-20",baselineAll$uniqueID)

#Check to see which baseline entries are not in x1 
baseline_hhid_not_in_X1<-c(sort(!baselineAll$hhid[(baselineAll$uniqueID %in% x1_data$uniqueID)]))
sort(baseline_hhid_not_in_X1[!(baseline_hhid_not_in_X1%in% MonthlyAll$hhid)])
#baseline_in_X1<-c(sort(baselineAll$hhid[(baselineAll$uniqueID %in% x1_data$uniqueID)]))
# sort(baseline_in_X1[!(baseline_in_X1%in% MonthlyAll$hhid)])

#check which baseline entries are in x1, can be used for analysis
baseline_in_X1<-c(sort(baselineAll$uniqueID[(baselineAll$uniqueID %in% x1_data$uniqueID)]))

# View(baselineAll[,c("hhid","uniqueID")])



baselineAll$uniqueID
# for (i in 1:10) { do something that involves i}
# i <- j <- 1 

final_x1_data <- data.frame()  

for (i in 1:length(unique(x1_data$HHID))){
  
  subset <- x1_data[which(x1_data$HHID==unique(x1_data$HHID)[i]),]
  subset <- subset[order(subset$base_date),]  
  
  for (j in 1:(nrow(subset)-1)) {
    subset$interval_check[j] <- subset$with_date[j] < subset$base_date[j+1]
  }
  
  subset$interval_check[j+1] <- TRUE
  final_x1_data <- rbind(final_x1_data, subset)
  
}

# final_x1_data[which(final_x1_data$interval_check==F),]


# First need to fix monthlyall date typos-----------------------------------------------------------------
setwd("C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data\\C5 Field Operations data\\X-2 Monthly visit tracking sheet")
X2<-read.csv2("X-2 monthly visits 4Jul15.csv")

X2$date<-as.Date(X2$Date.of.monthly.visit, "%d.%m.%y")
X2$uniqueDate<-paste(X2$HHID,"-",X2$date,sep="")

sort(monthly2$visitdate)

###############look into the following mess tomorrow##########################
# MonthlyAll$date <- with(MonthlyAll, as.Date(paste(year, "-", month, "-", day, sep="")))
# MonthlyAll$uniqueDate <- paste(MonthlyAll$hhid,"-",MonthlyAll$date,sep="")
# 
# sort(MonthlyAll$uniquedate[!(MonthlyAll$uniqueDate %in% X2$uniqueDate)])
# 
# monthlyall_not_in_X2<-sort(MonthlyAll$hhid[!(MonthlyAll$uniqueDate %in% X2$uniqueDate)])
# X2_not_in_monthlyall<-sort(X2$HHID[!(X2$uniqueDate %in% MonthlyAll$uniqueDate)])
# 
# X2$HHID[(X2_not_in_monthlyall %in% monthlyall_not_in_X2)]
# 
# Monthly_uniqueDates<-
# X2dates_uniqueDates


######################################################

MonthlyAll$uniqueid <- NA
MonthlyAll$uniqueid[which(MonthlyAll$hh_id==MonthlyAll$hh_id[1])] <- 
  
  final_x1_data$uniqueid[which((MonthlyAll$hh_id[1]==final_x1_data$HHID) &   
                                 (MonthlyAll$date[1] >= final_x1_data$base_date[which(final_x1_data$HHID==MonthlyAll$hh_id[1])] & 
                                    (MonthlyAll$date[1] <= final_x1_data$with_date[which(final_x1_data$HHID==MonthlyAll$hh_id[1])])))      
                         ]

