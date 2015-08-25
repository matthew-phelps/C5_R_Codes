# Author: Char Tamason & Matthew Phelps
# Desc:    Join most recent ODK data table and merge with older versions of survey
# output: Joined ODK table?


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mp <- "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK"
ct <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK"
setwd(mp)
rm(mp, ct)



# 1.) LOAD FILES --------------------------------------------------------------

# combining monthly visit files
# monthly 3 is exact duplicate of monthly 1
# monthly 4 is copy of 1_5, but 1_5 uses "," instead of "." with numbers and causes problems
# Monthly2 - monthly 6 covers time from Sep 9 2014 to June 12.
# Monthly7 covers June 15 - present.

monthly2<-read.csv2("C5_Monthlysurvey2.csv", stringsAsFactors=FALSE)
monthly3<-read.csv("C5_monthly_survey_v3_results.csv", stringsAsFactors=FALSE)
monthly4<-read.csv("C5_monthly_survey_v4_results.csv", stringsAsFactors=FALSE)
monthly5<-read.csv("C5_monthly_survey_v5_results_feb6tomar15_2015 downloaded apr17.csv", stringsAsFactors=FALSE)
monthly6<-read.csv("C5_monthly_survey_v6_results.csv", stringsAsFactors=FALSE)



# Create df with the  full file names and the date of modification.
# See http://stackoverflow.com/questions/13762224/how-to-sort-files-list-by-date/13762544
fileNames.df <- file.info(list.files(path = "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK",
                                     pattern = "C5_monthly_survey_v7_results.*\\.csv$", full.names = T)) # .*\\.csv$ is the pattern matching secton

# Sort by most modification time and extract the rowname of the most recently modified file:
fileNames.df <- fileNames.df[with(fileNames.df, order(as.POSIXct(mtime))), ]
monthly7.name <- fileNames.df[nrow(fileNames.df), ] # Subset the most recently modified file
monthly7.name <- rownames(monthly7.name) # Get file path
monthly7 <- read.csv(file = monthly7.name, stringsAsFactors = F) # turn into df
rm(monthly7.name, fileNames.df)




# 2.) VARIABLE SELECTION --------------------------------------------------

#make sure all columns in base name list exist in all monthly sets
names2<-names(monthly2)
names3<-names(monthly3)
names4<-names(monthly4)
names5<-names(monthly5)
names6<-names(monthly6)
names7<-names(monthly7)


# Variables to keep. If later during anlaysis we need to add more variables - add them to this list:

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



# Verify that each survery version has the required variables
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


# 3.) COMBINING DATASETS --------------------------------------------------
MonthlyAll<-rbind(monthly2_sub, monthly3_sub,monthly4_sub,monthly5_sub,
                  monthly6_sub,monthly7_sub)




# 4.) DATE OF VISIT -------------------------------------------------------

MonthlyAll$visitdate<-with(MonthlyAll, paste(day,"-", month,"-", year,sep=""))
MonthlyAll$visitdate<-as.Date(MonthlyAll$visitdate, "%d-%m-%Y")

# Move visit date column earlier so easier to read



#create date with auto_date from tablets
temp<-strsplit(MonthlyAll$auto_date, " ")
mat  <- matrix(unlist(temp), ncol=6, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("x", "month_auto", "day_auto", "y", "z", "year_auto")
MonthlyAll<-cbind(df,MonthlyAll)
MonthlyAll$visitdateauto<-paste(MonthlyAll$month_auto, MonthlyAll$day_auto, MonthlyAll$year_auto, sep="")
MonthlyAll$visitdateauto<-as.Date(MonthlyAll$visitdateauto, "%B%d%Y")

# sort(MonthlyAll$visitdate)

# Clean workspace.
rm(list = ls()[!(ls() %in% c('MonthlyAll'))])




#------------------code ends here, following is data cleaning------------------------------------

# Re-order columns so Date visit fields are first.
y <- match(c('visitdate', 'visitdateauto'), names(MonthlyAll))
x <- 1:(ncol(MonthlyAll) - length(y))
MonthlyAll.2 <- MonthlyAll[, c(y, x)]

# Remove duplicate columns.
dropVar <- c('x', 'month_auto', 'day_auto', 'y', 'z', 'year_auto', 'auto_date',
             'day', 'month', 'year')
MonthlyAll.2 <- MonthlyAll.2[, !names(MonthlyAll.2) %in% dropVar]
rm(y,x, dropVar)



# Relative numbers will change as data is added 
#duplicates records
duplicated(MonthlyAll[,2:5]) # TRUE = rows 41, 238; shows only the 2nd copy
duplicated(MonthlyAll[,2:5], fromLast=TRUE) #shows repeat from reverse to find the 1st copy
View(MonthlyAll[237,])
View(MonthlyAll[109,])
#erase duplicate (row 41)
MonthlyAll = MonthlyAll[-41,]
#need to delete row 237 or 109 depending on word from Bangladesh

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
summary(MonthlyAll$hh_id)
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
#check<-subset(MonthlyAll,as.numeric(cont1.cont1_size)>=40, select=c("hh_id", "month","day","year","FRA"))
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
                as.numeric(cont7.cont7_times)>=25, select=c("hh_id", "cont1.cont1_times", 
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
