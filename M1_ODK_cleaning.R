# Author: Char Tamason & Matthew Phelps
# Desc:    Join most recent ODK data table and merge with older versions of survey
# output: Joined ODK table?
# Dependencies: NONE



# Intro -------------------------------------------------------------------


 
# Prepare Matthew's workspace if user == MATTHEW. If else, setwd to Chars dir
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       data.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\month_all.Rdata",
       data.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata")
ifelse(grepl("zrc340", getwd()),
       ODK.path <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK",
       ODK.path <- "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK")


setwd(ODK.path)




# 1.) LOAD FILES --------------------------------------------------------------

# combining monthly visit files
# Monthly 2 set is entirely contained within monthly5_5. So we do not load monthly 2
# monthly 3 is exact duplicate of monthly 1
# monthly 4 is copy of 1_5, but 1_5 uses "," instead of "." with numbers and causes problems
# Monthly2 - monthly 6 covers time from Sep 9 2014 to June 12.
# Monthly7 covers June 15 - present.


monthly3<-read.csv("C5_monthly_survey_v3_results.csv", stringsAsFactors=FALSE)
monthly4<-read.csv("C5_monthly_survey_v4_results.csv", stringsAsFactors=FALSE)
monthly5<-read.csv("C5_monthly_survey_v5_results_feb6tomar15_2015 downloaded apr17.csv", stringsAsFactors=FALSE)
monthly5_5<-read.csv2("C5_monthly_survey_v5_5.csv")
monthly6<-read.csv("C5_monthly_survey_v6_results.csv", stringsAsFactors=FALSE)
## Monthly 7 needs to be dynamic:
# Create df with the  full file names and the date of modification.
# See http://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date/13762544
fileNames.df <- file.info(list.files(path = ODK.path,
                                     pattern = "C5_monthly_survey_v7_results.*\\.csv$", full.names = T))
# .*\\.csv$ is the pattern matching secton

# Sort files that match pattern by most modification time and 
# extract the rowname of the most recently modified file:
fileNames.df <- fileNames.df[with(fileNames.df, order(as.POSIXct(mtime))), ]
monthly7.name <- fileNames.df[nrow(fileNames.df), ] # Subset the most recently modified file
monthly7.path <- rownames(monthly7.name) # Get file path
monthly7 <- read.csv(file = monthly7.path, stringsAsFactors = F) # turn into df

# Store end.date for data validation (make sure no records are after the file creation date)
end.date <- as.Date(monthly7.name$mtime)
rm(monthly7.name, fileNames.df)




# 2.) VARIABLE SELECTION --------------------------------------------------

# Make sure all columns in base name list exist in all monthly sets

names3<-names(monthly3)
names4<-names(monthly4)
names5<-names(monthly5)
names5_5<-names(monthly5_5)
names6<-names(monthly6)
names7<-names(monthly7)

# Variables to keep. If later during anlaysis we need to add more variables - add them to this list:

base_name_list <- c("FRA", "hh_id", "auto_date","day","month", "year","first_visit", 'visit_num', 'survey_round', "num_wa_pts","water_point1.wa_pt1",
                    "water_point1.wa_pt1_usebefore",  "water_point1.wa_source1",  "water_point1.wa_source1_other",  
                    "water_point1.wa_tank1",  "water_point1.wa_tank1_other",	"water_point1.wa_avail1",
                    "water_point1.wa_flow1.wa_time1.aS",  "water_point1.wa_flow1.wa_time1.aE",	
                    "water_point1.wa_flow1.wa_time1a",	"water_point1.wa_flow1.wa_time1.bS",	
                    "water_point1.wa_flow1.wa_time1.bE",	"water_point1.wa_flow1.wa_time1b",	"water_point1.wa_flow1.wa_time1.cS",
                    "water_point1.wa_flow1.wa_time1.cE", "water_point1.wa_flow1.wa_time1c",	
                    "water_point2.wa_pt2",	"water_point2.wa_pt2_other",	"water_point2.wa_pt2_usebefore",	
                    "water_point2.wa_source2",	"water_point2.wa_source2_other",	"water_point2.wa_tank2",	
                    "water_point2.wa_tank2_other",	"water_point2.wa_avail2","wa_treated","wa_use", "visitor", "new_per",	
                    "new_per1_name",	"new_per1_age",	"new_per1_sex",	"new_per1_id",	"new_per2_name",	
                    "new_per2_age",	"new_per2_sex",	"new_per2_id",	"new_per3_name",	"new_per3_age",	
                    "new_per3_sex",	"new_per3_id", "old_per",  "old_per_out",	"old_per1_id",	"old_per2_id",	
                    "old_per3_id", "dia", "dia1",	"dia2", "latrine_feces",  "hh_feces",	"drain_feces",	
                    "hh_clean",	"bari_clean", "cont_use",  "cont1.cont1_id",	"cont1.cont1_size",	
                    "cont1.cont1_times",	"cont2.cont2_id",	"cont2.cont2_size",	"cont2.cont2_times",	
                    "cont3.cont3_id",	"cont3.cont3_size",	"cont3.cont3_times",	"cont4.cont4_id",	
                    "cont4.cont4_size",	"cont4.cont4_times",	"cont5.cont5_id",	"cont5.cont5_size",	
                    "cont5.cont5_times",	"cont6.cont6_id",	"cont6.cont6_size",	"cont6.cont6_times",
                    "cont7.cont7_id",	"cont7.cont7_size",	"cont7.cont7_times",	"cont8.cont8_id",	
                    "cont8.cont8_size",	"cont8.cont8_times",	"cont9.cont9_id",	"cont9.cont9_size",
                    "cont9.cont9_times",	"cont10.cont10_id",	"cont10.cont10_size",	"cont10.cont10_times",
                    "other_water_in.wash_hands_in", "other_water_in.adult_bathe_in",	"other_water_in.child_bathe_in",	
                    "other_water_in.wash_plate_in",	"other_water_in.wash_clothes_in", "other_water_out.wash_hands_out",  
                    "other_water_out.adult_bathe_out",	"other_water_out.child_bathe_out",	"other_water_out.wash_plate_out",	
                    "other_water_out.wash_clothes_out", "cont11.cont11_id",  "cont11.cont11_size",
                    "cont11.cont11_times",  "cont12.cont12_id",  "cont12.cont12_size",	"cont12.cont12_times",	
                    "cont13.cont13_id",	"cont13.cont13_size",	"cont13.cont13_times",	"cont14.cont14_id",	
                    "cont14.cont14_size",	"cont14.cont14_times",	"cont15.cont15_id",	"cont15.cont15_size",	
                    "cont15.cont15_times")



# Verify that each survery version has the required variables

base_name_list[!(base_name_list %in% names3)] #monthly3 is missing container 11-15 columns
base_name_list[!(base_name_list %in% names4)] # missing "visit_num"
base_name_list[!(base_name_list %in% names5)] # missing "visit_num"
base_name_list[!(base_name_list %in% names5_5)]
base_name_list[!(base_name_list %in% names6)]
base_name_list[!(base_name_list %in% names7)]

# To generate a list of missing columns (not needed for these data sets)

 
# Create dummy columns in datasets that are missing variables
monthly3$cont11.cont11_id<-as.character(NA)
monthly3$cont11.cont11_size<-as.numeric(NA)
monthly3$cont11.cont11_times<-as.numeric(NA)
monthly3$cont12.cont12_id<-as.character(NA)
monthly3$cont12.cont12_size<-as.numeric(NA)
monthly3$cont12.cont12_times<-as.numeric(NA)
monthly3$cont13.cont13_id<-as.character(NA)
monthly3$cont13.cont13_size<-as.numeric(NA)
monthly3$cont13.cont13_times<-as.numeric(NA)
monthly3$cont14.cont14_id<-as.character(NA)
monthly3$cont14.cont14_size<-as.numeric(NA)
monthly3$cont14.cont14_times<-as.numeric(NA)
monthly3$cont15.cont15_id<-as.character(NA)
monthly3$cont15.cont15_size<-as.numeric(NA)
monthly3$cont15.cont15_times<-as.numeric(NA)
monthly3$visit_num <- monthly4$visit_num <- monthly5$visit_num <- monthly5_5$visit_num <- as.numeric(NA)


# Create subsets with only wanted (base names) columns

mon3<-monthly3[,c(base_name_list)]
mon4<-monthly4[,c(base_name_list)]
mon5<-monthly5[,c(base_name_list)]
mon5_5<-monthly5_5[,c(base_name_list)]
mon6<-monthly6[,c(base_name_list)]
mon7<-monthly7[,c(base_name_list)]


# Remove un-needed datasets
rm(monthly7, monthly6, monthly5, monthly5_5, monthly4, monthly3)


# Add variable to track which version monthly visit comes from ------------

mon7$version <- 7
mon6$version <- 6
mon5$version <- 5
mon5_5$version <- 5.5
mon4$version <- 4
mon3$version <- 3

# 3.) COMBINING DATASETS --------------------------------------------------
MonthlyAll<-rbind(mon3,mon4,mon5, mon5_5,
                  mon6,mon7)

# 4.) DATE OF VISIT -------------------------------------------------------

MonthlyAll$visitdate<-with(MonthlyAll, paste(day,"-", month,"-", year,sep=""))
# MonthlyAll$visitdate
MonthlyAll$visitdate<-as.Date(MonthlyAll$visitdate, "%d-%m-%Y")

mon3$visitdate<-with(mon3, paste(day,"-", month,"-", year,sep=""))
mon3$visitdate<-as.Date(mon3$visitdate, "%d-%m-%Y")
mon4$visitdate<-with(mon4, paste(day,"-", month,"-", year,sep=""))
mon4$visitdate<-as.Date(mon4$visitdate, "%d-%m-%Y")
mon5$visitdate<-with(mon5, paste(day,"-", month,"-", year,sep=""))
mon5$visitdate<-as.Date(mon5$visitdate, "%d-%m-%Y")
mon6$visitdate<-with(mon6, paste(day,"-", month,"-", year,sep=""))
mon6$visitdate<-as.Date(mon6$visitdate, "%d-%m-%Y")
mon7$visitdate<-with(mon7, paste(day,"-", month,"-", year,sep=""))
mon7$visitdate<-as.Date(mon7$visitdate, "%d-%m-%Y")
mon5_5$visitdate<-with(mon5_5, paste(day,"-", month,"-", year,sep=""))
mon5_5$visitdate<-as.Date(mon5_5$visitdate, "%d-%m-%Y")

#create date with auto_date from tablets
temp<-strsplit(MonthlyAll$auto_date, " ")
mat  <- matrix(unlist(temp), ncol=6, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("x", "month_auto", "day_auto", "y", "z", "year_auto")
MonthlyAll<-cbind(df,MonthlyAll)
MonthlyAll$visitdateauto<-paste(MonthlyAll$month_auto, MonthlyAll$day_auto, MonthlyAll$year_auto, sep="")
MonthlyAll$visitdateauto<-as.Date(MonthlyAll$visitdateauto, "%B%d%Y")

#MonthlyAll$visitdateauto[1]
# sort(MonthlyAll$visitdate)


# 5.) CLEANING EACH MONTHLY DATASET ---------------------------------------


# # Sort x2 by date so we can compare to monthly visit files:
# x2 <- x2[with(x2, order(HH_baseline, date.monthly.visit)), ]
# x2$HHID <- as.numeric(x2$HHID)


# Monthly 3: Check & Change date anomolies.
boxplot(mon3$visitdate)
head(sort(mon3$visitdate), 7)
tail(sort(mon3$visitdate), 7) 
# range 2014-09-09 to 2014-11-25, anomalies: 2014-08-15, 2015-09-25

# Make Changes
mon3$visitdate[mon3$visitdate=="2015-09-25"]<-as.Date("2014-09-25")
mon3$hh_id[mon3$visitdate=="2014-08-15"] #HHID 398
mon3$visitdate[mon3$hh_id==398] #"2014-11-07" "2014-08-15"
# x2$date[x2$HHID==398] #"2014-09-16" is the closest and is not repeated above
mon3$visitdate[mon3$visitdate=="2014-08-15"]<-as.Date("2014-09-16")


# Monthly 4: Check & Change date anomolies.
sort(mon4$visitdate) # range 2014-09-20 to 2014-11-26, no anomalies
boxplot(mon4$visitdate)

# Monthly 5_5: Check & Change date anomolies.
sort(mon5_5$visitdate) #range 2014-10-13 to 2015-01-03, anomalies:  2015-01-14
mon5_5$hh_id[mon5_5$visitdate=="2015-01-14"] #hhid 400

MonthlyAll$visitdate[MonthlyAll$hh_id==400] #"2015-01-14" "2014-11-13" "2014-09-29" "2015-02-13" "2015-01-14" "2015-03-20" "2015-05-08"
# x2$date[x2$HHID==400] #"2014-09-29" "2014-11-14" "2015-02-13" "2015-03-20" "2015-05-08" "2015-06-19"
#no changes, a duplicate will be deleted later


# Monthly 5: Check & Change date anomolies.
boxplot(mon5$visitdate)
head(sort(mon5$visitdate))
tail(sort(mon5$visitdate))
# range 2015-02-03 to 2015-03-15, anomalies: "2014-12-26", "2015-01-02", "2016-02-18"

mon5$visitdate[mon5$visitdate=="2016-02-18"]<- as.Date("2015-02-18") 

mon5$hh_id[mon5$visitdate=="2014-12-26"] #hhid 297
MonthlyAll$visitdate[MonthlyAll$hh_id==297] #"2014-11-16" "2014-12-26" "2015-03-30" "2015-05-26"
# x2$date[x2$HHID==297]  #"2014-11-21" "2015-02-13" "2015-03-30" "2015-05-26 
mon5$visitdate[mon5$visitdate=="2014-12-26"]<- as.Date("2015-02-13") ## Hmm, are we sure?

mon5$hh_id[mon5$visitdate=="2015-01-02"]
MonthlyAll$visitdate[MonthlyAll$hh_id==207] #"2014-11-28" "2015-01-02" "2015-04-23" "2015-05-27"
# x2$date[x2$HHID==207]  #"2014-11-28" "2015-02-06" "2015-04-23" "2015-05-27" "2015-07-03"
mon5$visitdate[mon5$visitdate=="2015-01-02"]<- as.Date("2015-02-06") 


# Monthly 6: Check & Change date anomolies.
boxplot(mon6$visitdate)
head(sort(mon6$visitdate))
tail(sort(mon6$visitdate))
# range 2015-03-15 to 2015-06-12, anomalies: 2014-04-07, 2015-02-01, 2016-05-11, "2015-07-08" "2015-08-12"

mon6$visitdate[mon6$visitdate=="2014-04-07"]<-as.Date("2015-04-07")
mon6$visitdate[mon6$visitdate=="2016-05-11"]<-as.Date("2015-05-11")

mon6$hh_id[mon6$visitdate=="2015-02-01"] #hhid 196
MonthlyAll$visitdate[MonthlyAll$hh_id==196] #"2014-12-10" "2015-02-23" "2015-02-01" "2015-06-04"
# x2$date[x2$HHID==196] #"2014-12-10" "2015-02-23" "2015-04-01" "2015-06-04"
mon6$visitdate[mon6$visitdate=="2015-02-01"]<-as.Date("2015-04-01")

mon6$hh_id[mon6$visitdate=="2015-07-08"] #hhid 85
MonthlyAll$visitdate[MonthlyAll$hh_id==85] #  "2014-09-20" "2014-11-11" "2015-02-16" "2015-03-24" "2015-07-08"
# x2$date[x2$HHID==085] # "2014-09-20" "2014-11-11" "2015-02-16" "2015-03-24" "2015-03-04" "2015-04-22" "2015-05-08" "2015-06-23"
mon6$visitdate[mon6$visitdate=="2015-07-08"]<-as.Date("2015-05-08")

mon6$hh_id[mon6$visitdate=="2015-08-12"] #hhid347
MonthlyAll$visitdate[MonthlyAll$hh_id==347] #"2014-12-11" "2015-02-07" "2014-12-11" "2015-03-16" "2015-08-12"
# x2$date[x2$HHID==347] # "2014-12-11" "2015-02-07" "2015-03-16" "2015-05-12"
mon6$visitdate[mon6$visitdate=="2015-08-12"]<-as.Date("2015-05-12")


#  Monthly 7: Check & Change date anomolies.------------------------------------------------------------------------
 
boxplot(mon7$visitdate)
head(sort(mon7$visitdate))
tail(sort(mon7$visitdate))
# range 2015-06-12 to 2015-06-15, anomaly: "2014-07-06" "2015-04-04" "2016-07-09" "2016-07-13"

start.date <- as.Date("2015-06-07")
end.date # created in Step 1.)
early <- mon7[mon7$visitdate < start.date, ]
late <- mon7[mon7$visitdate > end.date, ]

y <- mon7$hh_id[mon7$visitdate=="2014-07-06"]
MonthlyAll$visitdate[MonthlyAll$hh_id==y]
# x2$date[x2$HHID==y]
mon7$visitdate[mon7$visitdate == '2014-07-06'] <- as.Date('2015-07-06')

y <- mon7$hh_id[mon7$visitdate=="2015-04-04"] #hhid269
MonthlyAll[MonthlyAll$hh_id==y,] 
# x2$date[x2$HHID==y]
#### doesn't work: mon7[which(mon7$visitdate=="2015-04-04"& mon7$hh_id==123),]<-NULL 
#### # simply doesn't make sense in this date range for monthly 7, no in line with previous monthly visit dates, and doesn't match and  date
## Matt's guess: the hhid was recorded incorrectly- this belongs to another house

y <- mon7$hh_id[mon7$visitdate=="2016-07-09"]
MonthlyAll$visitdate[MonthlyAll$hh_id==y]
# x2$date[x2$HHID==y]
mon7$visitdate[mon7$visitdate == '2016-07-09'] <- as.Date('2015-07-09')

y <- mon7$hh_id[mon7$visitdate=="2016-07-13"]
MonthlyAll$visitdate[MonthlyAll$hh_id==y]
# x2$date[x2$HHID==y]
mon7$visitdate[mon7$visitdate == '2016-07-13'] <- as.Date('2015-07-13')

y <- mon7$hh_id[mon7$visitdate=="2015-04-04"]
MonthlyAll$visitdate[MonthlyAll$hh_id==y]

# Re-check to see if any new entries need cleaning:
boxplot(mon7$visitdate)
head(sort(mon7$visitdate))
tail(sort(mon7$visitdate))

# 6.) MAKE CLEAN JOIN ----------------------------------------------------------

MonthlyAll<-rbind(mon3,mon4,mon5, mon5_5,
                  mon6,mon7)

###Changes from Bimal's checks at field station
MonthlyAll$cont1.cont1_times<-with(MonthlyAll,ifelse(hh_id==9&month==9&day==20&year==2014,36,cont1.cont1_times))
MonthlyAll$cont1.cont1_times<-with(MonthlyAll,ifelse(hh_id==89&month==2&day==26&year==2015,6,cont1.cont1_times))
MonthlyAll$cont1.cont1_times<-with(MonthlyAll,ifelse(hh_id==89&month==4&day==2&year==2015,7,cont1.cont1_times))
#delete a row that was slightly different than another, FRA 9947 is correct, FRA 6889 is not
MonthlyAll=MonthlyAll[!((MonthlyAll$FRA==6889)&(MonthlyAll$hh_id==147)&(MonthlyAll$month==10)&(MonthlyAll$day==21)&(MonthlyAll$year==2014)),]
#change mistakes in container entries for hhid 151
MonthlyAll$water_point1.wa_pt1<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),4,water_point1.wa_pt1))
MonthlyAll$num_wa_pts<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),1,num_wa_pts))
MonthlyAll$water_point1.wa_pt1_usebefore<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),1,water_point1.wa_pt1_usebefore))
MonthlyAll$water_point1.wa_avail1<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),3,water_point1.wa_avail1))
MonthlyAll$water_point1.wa_flow1.wa_time1.aS<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),"18:00:00",water_point1.wa_flow1.wa_time1.aS))
MonthlyAll$water_point1.wa_flow1.wa_time1.aE<-with(MonthlyAll, ifelse((hh_id==151)&(month==3)&(day==3)&(year==2015),"17:59:00",water_point1.wa_flow1.wa_time1.aE))

# Check data
boxplot(MonthlyAll$visitdate)
min(MonthlyAll$visitdate)
max(MonthlyAll$visitdate)

# 7.) MAKE DATA FOR HUMAN READIBLE ---------------------------------------------------
dropVar <- c('x', 'month_auto', 'day_auto', 'y', 'z', 'year_auto', 'auto_date',
             'day', 'month', 'year')
MonthlyAll <- MonthlyAll[, !names(MonthlyAll) %in% dropVar]

y <- match(c('visitdate'), names(MonthlyAll))
x <- 1:ncol(MonthlyAll)
x <- x[-c(y)]
MonthlyAll <- MonthlyAll[, c(y, x)]
rm(x, y)

MonthlyAll <- MonthlyAll[order(MonthlyAll$hh_id, MonthlyAll$visitdate), ]
row.names(MonthlyAll) <- NULL

# Clean misentered HHIDs and dates  ---------------------------------------

MonthlyAll[MonthlyAll$hh_id==241&MonthlyAll$visitdate=="2015-04-27","hh_id"]<-247
MonthlyAll[MonthlyAll$hh_id==266&MonthlyAll$visitdate=="2015-01-03","hh_id"]<-191
MonthlyAll[MonthlyAll$hh_id==283&MonthlyAll$visitdate=="2014-11-10","hh_id"]<-243
MonthlyAll[MonthlyAll$hh_id==018&MonthlyAll$visitdate=="2015-08-05","hh_id"]<-038
MonthlyAll[MonthlyAll$hh_id==058&MonthlyAll$visitdate=="2015-03-23","hh_id"]<-054
MonthlyAll[MonthlyAll$hh_id==064&MonthlyAll$visitdate=="2015-08-19","hh_id"]<-074
MonthlyAll[MonthlyAll$hh_id==078&MonthlyAll$visitdate=="2014-10-28","hh_id"]<-074
MonthlyAll[MonthlyAll$hh_id==146&MonthlyAll$visitdate=="2015-08-10","hh_id"]<-136
MonthlyAll[MonthlyAll$hh_id==227&MonthlyAll$visitdate=="2015-03-20","hh_id"]<-272
MonthlyAll[MonthlyAll$hh_id==258&MonthlyAll$visitdate=="2014-12-11","hh_id"]<-285
MonthlyAll[MonthlyAll$hh_id==290&MonthlyAll$visitdate=="2015-03-19","hh_id"]<-204
MonthlyAll[MonthlyAll$hh_id==317&MonthlyAll$visitdate=="2014-10-31","hh_id"]<-337
MonthlyAll[MonthlyAll$hh_id==327&MonthlyAll$visitdate=="2015-02-13","hh_id"]<-372

MonthlyAll[MonthlyAll$hh_id==337&MonthlyAll$visitdate=="2014-10-31","visitdate"]<-as.Date("2014-11-01")
MonthlyAll[MonthlyAll$hh_id==209&MonthlyAll$visitdate=="2014-12-19","visitdate"]<-as.Date("2014-12-22")
MonthlyAll[MonthlyAll$hh_id==222&MonthlyAll$visitdate=="2015-09-04","visitdate"]<-as.Date("2015-09-07")
MonthlyAll[MonthlyAll$hh_id==270&MonthlyAll$visitdate=="2014-11-21","visitdate"]<-as.Date("2014-12-17")
MonthlyAll[MonthlyAll$hh_id==297&MonthlyAll$visitdate=="2014-11-16","visitdate"]<-as.Date("2014-11-21")
MonthlyAll[MonthlyAll$hh_id==395&MonthlyAll$visitdate=="2014-10-09","visitdate"]<-as.Date("2014-11-10")
MonthlyAll<-MonthlyAll[!(MonthlyAll$hh_id==028&MonthlyAll$visitdate=="2014-10-20"),]
MonthlyAll$hh_id[MonthlyAll$hh_id==024&MonthlyAll$visitdate=="2014-11-18"]<-028


#MonthlyAll[MonthlyAll$hh_id==337&MonthlyAll$visitdate=="2014-10-31","visitdate"] # check that entry exists as written
#MonthlyAll[MonthlyAll$hh_id==297&MonthlyAll$visitdate=="2015-04-27",]

# SAVE DATA TO DISK -------------------------------------------------------

save(MonthlyAll, file = data.path)



