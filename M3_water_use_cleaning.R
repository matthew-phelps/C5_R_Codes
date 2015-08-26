# Author: Char Tamason
# Desc:    Cleaning Water-use related data
# output: Water-cleaned monthly data


# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mp <- "C:/Users/wrz741/Dropbox/C5 Monthly Visits Data/Raw data direct from ODK"
ct <- "C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Monthly Visits Data/Raw data direct from ODK"
data.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\month_all.Rdata"
setwd(mp)
rm(mp, ct)

source("C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\M1_monthly_joins.R") # PATH TO M1 HERE
source("C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\M2_monthly_cleaning.R") # PATH TO M2 HERE

#data cleaning by summary

summary(MonthlyAll$visitdate)
summary(MonthlyAll$first_visit)
summary(MonthlyAll$num_wa_pts)

nrow(MonthlyAll[MonthlyAll$num_wa_pts==0,])
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
