
#combining monthly visit files
setwd("C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK")
#monthly 3 is exact duplicate of monthly 1
#monthly 4 is copy of 1_5, but 1_5 uses "," instead of "." with numbers and causes problems

monthly2<-read.csv2("C5_Monthlysurvey2.csv", stringsAsFactors=FALSE)
monthly3<-read.csv("C5_monthly_survey_v3_results.csv", stringsAsFactors=FALSE)
monthly4<-read.csv("C5_monthly_survey_v4_results.csv", stringsAsFactors=FALSE)
monthly5<-read.csv("C5_monthly_survey_v5_results_feb6tomar15_2015 downloaded apr17.csv", stringsAsFactors=FALSE)
monthly5_5<-read.csv2("C5_monthly_survey_v5_5.csv")
monthly6<-read.csv("C5_monthly_survey_v6_results.csv", stringsAsFactors=FALSE)
monthly7<-read.csv("C5_monthly_survey_v7_results downloaded 16-7-15.csv", stringsAsFactors=FALSE) 
###monthly7 file name will need to be updated


monthly2<-as.data.frame(monthly2) 
monthly3<-as.data.frame(monthly3)
monthly4<-as.data.frame(monthly4)
monthly5<-as.data.frame(monthly5)
monthly6<-as.data.frame(monthly6)
monthly7<-as.data.frame(monthly7)
monthly5_5<-as.data.frame(monthly5_5)


# initial baseline of names

base_name_list <- c("FRA", "hh_id", "auto_date","day","month", "year","first_visit","num_wa_pts","water_point1.wa_pt1",
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


#make sure all columns in base name list exist in all monthly sets
names2<-names(monthly2)
names3<-names(monthly3)
names4<-names(monthly4)
names5<-names(monthly5)
names6<-names(monthly6)
names7<-names(monthly7)

base_name_list[!(base_name_list %in% names2)]
base_name_list[!(base_name_list %in% names3)] #monthly3 is missing container 11-15 columns
base_name_list[!(base_name_list %in% names4)]
base_name_list[!(base_name_list %in% names5)]
base_name_list[!(base_name_list %in% names6)]
base_name_list[!(base_name_list %in% names7)]

#to generate a list of missing columns (not needed for these data sets)
missing_col_list <- names(names2)[!(names2 %in% base_name_list)]
missing_data <- names2[missing_col_list]

#create dummy columns in monthly 3
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

#create subsets with only wanted (base names) columns
monthly2_sub<-monthly2[,c(base_name_list)]
monthly3_sub<-monthly3[,c(base_name_list)]
monthly4_sub<-monthly4[,c(base_name_list)]
monthly5_sub<-monthly5[,c(base_name_list)]
monthly6_sub<-monthly6[,c(base_name_list)]
monthly7_sub<-monthly7[,c(base_name_list)]
monthly5_5_sub<-monthly5_5[,c(base_name_list)]


#create visit dates for each dataset so the range can be checked for data cleaning
monthly2_sub$visitdate<-with(monthly2_sub, paste(day,"-", month,"-", year,sep=""))
monthly2_sub$visitdate<-as.Date(monthly2_sub$visitdate, "%d-%m-%Y")
monthly3_sub$visitdate<-with(monthly3_sub, paste(day,"-", month,"-", year,sep=""))
monthly3_sub$visitdate<-as.Date(monthly3_sub$visitdate, "%d-%m-%Y")
monthly4_sub$visitdate<-with(monthly4_sub, paste(day,"-", month,"-", year,sep=""))
monthly4_sub$visitdate<-as.Date(monthly4_sub$visitdate, "%d-%m-%Y")
monthly5_sub$visitdate<-with(monthly5_sub, paste(day,"-", month,"-", year,sep=""))
monthly5_sub$visitdate<-as.Date(monthly5_sub$visitdate, "%d-%m-%Y")
monthly6_sub$visitdate<-with(monthly6_sub, paste(day,"-", month,"-", year,sep=""))
monthly6_sub$visitdate<-as.Date(monthly6_sub$visitdate, "%d-%m-%Y")
monthly7_sub$visitdate<-with(monthly7_sub, paste(day,"-", month,"-", year,sep=""))
monthly7_sub$visitdate<-as.Date(monthly7_sub$visitdate, "%d-%m-%Y")
monthly5_5_sub$visitdate<-with(monthly5_5_sub, paste(day,"-", month,"-", year,sep=""))
monthly5_5_sub$visitdate<-as.Date(monthly5_5_sub$visitdate, "%d-%m-%Y")

#sort date ranges and replace anamolies
sort(monthly2_sub$visitdate) # range 2014-11-27 to 2014-12-19 anomalies: 2014-10-30, 2014-11-16, 2014-12-22, 2015-01-14

#monthly3 checks and changes
sort(monthly3_sub$visitdate) # range 2014-09-09 to 2014-11-25, anomalies: 2014-08-15, 2015-09-25
monthly3_sub$visitdate[monthly3_sub$visitdate=="2015-09-25"]<-"2014-09-25"
monthly3_sub$hh_id[monthly3_sub$visitdate=="2014-08-15"] #HHID 398
monthly3_sub$visitdate[monthly3_sub$hh_id==398] #"2014-11-07" "2014-08-15"
X2$date[X2$HHID==398] #"2014-09-16" is the closest and is not repeated above
monthly3_sub$visitdate[monthly3_sub$visitdate=="2014-08-15"]<-"2014-09-16"

#monthly4 checks and changes
sort(monthly4_sub$visitdate) # range 2014-09-20 to 2014-11-26

#monthly5_5
sort(monthly5_5_sub$visitdate) #range 2014-10-13 to 2015-01-03, anomalies:  2015-01-14
monthly5_5_sub$hh_id[monthly5_5_sub$visitdate=="2015-01-14"] #hhid 400

MonthlyAll$visitdate[MonthlyAll$hh_id==400] #"2015-01-14" "2014-11-13" "2014-09-29" "2015-02-13" "2015-01-14" "2015-03-20" "2015-05-08"
X2$date[X2$HHID==400] #"2014-09-29" "2014-11-14" "2015-02-13" "2015-03-20" "2015-05-08" "2015-06-19"
#no changes, a duplicate will be deleted later


#monthly5 checks and changes
sort(X2$date)
sort(monthly5_sub$visitdate) # range 2015-02-03 to 2015-03-15, anomalies: "2014-12-26", "2015-01-02", "2016-02-18"
monthly5_sub$visitdate[monthly5_sub$visitdate=="2016-02-18"]<- "2015-02-18" 

monthly5_sub$hh_id[monthly5_sub$visitdate=="2014-12-26"] #hhid 297
MonthlyAll$visitdate[MonthlyAll$hh_id==297] #"2014-11-16" "2014-12-26" "2015-03-30" "2015-05-26"
X2$date[X2$HHID==297]  #"2014-11-21" "2015-02-13" "2015-03-30" "2015-05-26 
monthly5_sub$visitdate[monthly5_sub$visitdate=="2014-12-26"]<- "2015-02-13"

monthly5_sub$hh_id[monthly5_sub$visitdate=="2015-01-02"]
MonthlyAll$visitdate[MonthlyAll$hh_id==207] #"2014-11-28" "2015-01-02" "2015-04-23" "2015-05-27"
X2$date[X2$HHID==207]  #"2014-11-28" "2015-02-06" "2015-04-23" "2015-05-27" "2015-07-03"
monthly5_sub$visitdate[monthly5_sub$visitdate=="2015-01-02"]<- "2015-02-06" 

#monthly6 date checks and replacements
sort(monthly6_sub$visitdate) # range 2015-03-15 to 2015-06-12, anomalies: 2014-04-07, 2015-02-01, 2016-05-11, "2015-07-08" "2015-08-12"
monthly6_sub$visitdate[monthly6_sub$visitdate=="2014-04-07"]<-"2015-04-07"
monthly6_sub$visitdate[monthly6_sub$visitdate=="2016-05-11"]<-"2015-05-11"

monthly6_sub$hh_id[monthly6_sub$visitdate=="2015-02-01"] #hhid 196
MonthlyAll$visitdate[MonthlyAll$hh_id==196] #"2014-12-10" "2015-02-23" "2015-02-01" "2015-06-04"
X2$date[X2$HHID==196] #"2014-12-10" "2015-02-23" "2015-04-01" "2015-06-04"
monthly6_sub$visitdate[monthly6_sub$visitdate=="2015-02-01"]<-"2015-04-01"

monthly6_sub$hh_id[monthly6_sub$visitdate=="2015-07-08"] #hhid 85
MonthlyAll$visitdate[MonthlyAll$hh_id==85] #  "2014-09-20" "2014-11-11" "2015-02-16" "2015-03-24" "2015-07-08"
X2$date[X2$HHID==85] # "2014-09-20" "2014-11-11" "2015-02-16" "2015-03-24" "2015-03-04" "2015-04-22" "2015-05-08" "2015-06-23"
monthly6_sub$hh_id[monthly6_sub$visitdate=="2015-07-08"]<-"2015-05-08"

monthly6_sub$hh_id[monthly6_sub$visitdate=="2015-08-12"] #hhid347
MonthlyAll$visitdate[MonthlyAll$hh_id==347] #"2014-12-11" "2015-02-07" "2014-12-11" "2015-03-16" "2015-08-12"
X2$date[X2$HHID==347] # "2014-12-11" "2015-02-07" "2015-03-16" "2015-05-12"
monthly6_sub$hh_id[monthly6_sub$visitdate=="2015-08-12"]<-"2015-05-12"

#monthly7 check dates and replace
sort(monthly7_sub$visitdate) # range 2015-06-12 to 2015-06-15, anomaly:2015-04-04
monthly7_sub$hh_id[monthly7_sub$visitdate=="2015-04-04"] #hhid269
MonthlyAll$visitdate[MonthlyAll$hh_id==123] #"2014-10-16" "2014-11-26" "2015-03-15" "2015-04-07" "2015-05-12" "2015-04-04"
X2$date[X2$HHID==123] #"2014-10-16" "2014-11-26" "2015-04-07" "2015-05-12" "2015-06-26"
#### doesn't work: monthly7_sub[which(monthly7_sub$visitdate=="2015-04-04"& monthly7_sub$hh_id==123),]<-NULL # simply doesn't make sense in this date range for monthly 7, no in line with previous monthly visit dates, and doesn't match and X2 date

#combine datasets 
MonthlyAll<-rbind(monthly2_sub, monthly3_sub,monthly4_sub,monthly5_sub,monthly5_5_sub,
                  monthly6_sub,monthly7_sub)

MonthlyAll$visitdate<-with(MonthlyAll, paste(day,"-", month,"-", year,sep=""))
MonthlyAll$visitdate<-as.Date(MonthlyAll$visitdate, "%d-%m-%Y")

#rename HHID variable so it matches baseline
colnames(MonthlyAll)[8]<-"hhid"

#create date with auto_date from tablets
temp<-strsplit(MonthlyAll$auto_date, " ")
mat  <- matrix(unlist(temp), ncol=6, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("x", "month_auto", "day_auto", "y", "z", "year_auto")
MonthlyAll<-cbind(df,MonthlyAll)
MonthlyAll$visitdateauto<-paste(MonthlyAll$month_auto, MonthlyAll$day_auto, MonthlyAll$year_auto, sep="")
MonthlyAll$visitdateauto<-as.Date(MonthlyAll$visitdateauto, "%B%d%Y")
# sort(MonthlyAll$visitdate)

#------------------code ends here, following is data cleaning------------------------------------

#Data cleaning
#duplicates
MonthlyAll$auto_date<-NULL #erased to get 4 columns in a row to check for duplicates in next line
duplicated(MonthlyAll[,8:11]) # TRUE = rows 41 and 238; shows only the 2nd copy
duplicated(MonthlyAll[,8:11], fromLast=TRUE) #shows repeat from reverse to find the 1st copy rows 20 and 110
View(MonthlyAll[238,])
View(MonthlyAll[110,])
#erase duplicate (row 41)
MonthlyAll = MonthlyAll[-41,]
#need to delete row 23 or 110 depending on word from Bangladesh

#make sure dates of visits make sense
sort(MonthlyAll$visitdate) #monthly visits started on sept 9,2014, "2014-04-07" and "2014-08-15" are earlier, "2016-02-18" "2016-05-11" are later
# MonthlyAll$visitdateauto[MonthlyAll$visitdate=="2014-04-07"] # is "2015-04-07", change it
MonthlyAll$visitdate<-ifelse(MonthlyAll$visitdate=="2014-04-07","2015-04-07",MonthlyAll$visitdate)
# MonthlyAll$visitdateauto[MonthlyAll$visitdate=="2014-08-15"] #is "2014-11-10", based on order of those autodates (below) change to "2014-09-15"
# MonthlyAll$visitdate[MonthlyAll$visitdateauto=="2014-11-10"]
MonthlyAll$visitdate<-ifelse(MonthlyAll$visitdate=="2014-08-15","2014-09-15",MonthlyAll$visitdate)
MonthlyAll$visitdate<-ifelse(MonthlyAll$visitdate=="2016-02-18" ,"2015-02-18" ,MonthlyAll$visitdate)
MonthlyAll$visitdate<-ifelse(MonthlyAll$visitdate=="2016-05-11","2015-05-11",MonthlyAll$visitdate)

#data cleaning by summary
summary(MonthlyAll$hhid)
summary(MonthlyAll$day)
summary(MonthlyAll$month) # min =1, max = 12

summary(MonthlyAll$year)
MonthlyAll[MonthlyAll$year==2016,] 
#replace 2016 with 2015 in the 2 returned cases
MonthlyAll[MonthlyAll$year==2016,]<-2015

summary(MonthlyAll$first_visit)
summary(MonthlyAll$num_wa_pts)
MonthlyAll[MonthlyAll$num_wa_pts==0,]
MonthlyAll[MonthlyAll$num_wa_pts==10,]

MonthlyAll$daily_volume<-as.numeric(MonthlyAll$cont1.cont1_size)*(as.numeric(MonthlyAll$cont1.cont1_times))

summary(as.numeric(MonthlyAll$cont1.cont1_size))
MonthlyAll[MonthlyAll$cont1.cont1_size==56,]
#which((MonthlyAll$cont1.cont1_size*MonthlyAll$cont1.cont1_times)>=200)
#check<-subset(MonthlyAll,as.numeric(cont1.cont1_size)>=40, select=c("hhid", "month","day","year","FRA"))
summary(as.numeric(MonthlyAll$cont2.cont2_size))
summary(as.numeric(MonthlyAll$cont3.cont3_size))
summary(as.numeric(MonthlyAll$cont4.cont4_size))
summary(as.numeric(MonthlyAll$cont5.cont5_size))
summary(as.numeric(MonthlyAll$cont6.cont6_size))
summary(as.numeric(MonthlyAll$cont7.cont7_size))
summary(as.numeric(MonthlyAll$cont8.cont8_size))
summary(as.numeric(MonthlyAll$cont9.cont9_size))   
summary(as.numeric(MonthlyAll$cont10.cont10_size))
summary(as.numeric(MonthlyAll$cont11.cont11_size))
summary(as.numeric(MonthlyAll$cont12.cont12_size))
summary(as.numeric(MonthlyAll$cont13.cont13_size))
summary(as.numeric(MonthlyAll$cont14.cont14_size))
summary(as.numeric(MonthlyAll$cont15.cont15_size))

summary(as.numeric(MonthlyAll$cont1.cont1_times))
check<-subset(MonthlyAll, as.numeric(cont1.cont1_times)>=25|
                as.numeric(cont2.cont2_times)>=25 |as.numeric(cont3.cont3_times)>=25 |
                as.numeric(cont4.cont4_times)>=25 |as.numeric(cont6.cont6_times)>=25 |
                as.numeric(cont7.cont7_times)>=25, select=c("hhid", "cont1.cont1_times", 
                                                            "cont1.cont1_size", "cont2.cont2_times", "cont2.cont2_size","cont3.cont3_times", 
                                                            "cont3.cont3_size","cont4.cont4_times", "cont4.cont4_size","cont6.cont6_times", 
                                                            "cont6.cont6_size","cont7.cont7_times", "cont7.cont7_size","month","day","year","FRA"))
View(check)
write.csv(check, file="Too many container refils.csv")
summary(as.numeric(MonthlyAll$cont2.cont2_times))
summary(as.numeric(MonthlyAll$cont3.cont3_times))
summary(as.numeric(MonthlyAll$cont4.cont4_times))
summary(as.numeric(MonthlyAll$cont5.cont5_times))
summary(as.numeric(MonthlyAll$cont6.cont6_times))
summary(as.numeric(MonthlyAll$cont7.cont7_times))
summary(as.numeric(MonthlyAll$cont8.cont8_times))
summary(as.numeric(MonthlyAll$cont9.cont9_times))   
summary(as.numeric(MonthlyAll$cont10.cont10_times))
summary(as.numeric(MonthlyAll$cont11.cont11_times))
summary(as.numeric(MonthlyAll$cont12.cont12_times))
summary(as.numeric(MonthlyAll$cont13.cont13_times))
summary(as.numeric(MonthlyAll$cont14.cont14_times))
summary(as.numeric(MonthlyAll$cont15.cont15_times))
head(MonthlyAll)
summary(MonthlyAll$first_visit) #should be min = 1, max = 2
table(MonthlyAll$num_wa_pts)
table(MonthlyAll$water_point1.wa_pt1) #should add to # of observations
table(MonthlyAll$water_point2.wa_pt2) #should add to # of observations - water point 1 = 1617-790 = 827
