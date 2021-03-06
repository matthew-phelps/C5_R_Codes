#baseline descriptives

library(chron)
library(lme4)

# Load data ---------------------------------------------------------------
ifelse(grepl("zrc340", getwd()), 
       Q11.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\Q11_all.Rdata",
       Q11.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Q11_all.Rdata")
ifelse(grepl("zrc340", getwd()), 
       m4.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\clean-monthly-baseline.Rdata",
       m4.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()), 
       data.out.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\monthly-water.Rdata",
       data.out.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-water.Rdata")

load(m4.path)
load(Q11.path)

#subset only data from monthly visits (gets ride of unmatched x-2 entries)
monthly<-m4[which(!(is.na(m4$FRA))),]
monthly<-as.data.frame(monthly)
monthly[is.na(monthly)]<-0

#variables that will be used
monthly$month1<-formatC(monthly$month,width=2,format='d', flag = 0)

monthly$year.month<-as.numeric(with(monthly, ifelse(month1=="09"|month1=="10"|month1=="11"|
                              month1=="12",paste("14.",month,sep=""),paste("15.",month1,sep=""))))

monthly$season<-with(monthly, ifelse(month1=="01"|month1=="02"|month1=="03",1,
                                     ifelse(month1=="04"|month1=="05"|month1=="06",2,
                                            ifelse(month1=="07"|month1=="08"|month1=="09",3,4))))

monthly$ppl<-ifelse(monthly$ppl==0,monthly$total_HH_members,monthly$ppl)
monthly$daily_h2o_percapita<-with(monthly, daily_volume/ppl)

# Water access groups -----------------------------------------------------
#
#q14_recoded, 1 = pipe/tap, 2= hand pump, 3= well with bucket
#q14a_recoded, WASA=1; deep tube well/submersible =2,3 <-2 ; well/shallow tube well = 4,5 <-3;
#q15_recoded, is there a tank? 1=yes, 0=no
#use_bucket, is a bucket needed to withdraw water? 1=yes, 0=no
#m4$distance_to_source1, anything over 20 meters =21
#m4$daily_volume<-as.numeric(m4$cont1.cont1_size)*(as.numeric(m4$cont1.cont1_times))

#first create month variable
monthly$date_visit_character<-as.character(monthly$date_visit)
temp<-strsplit(monthly$date_visit_character, "-")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("year", "month", "day")
monthly<- cbind(monthly,df)
#for primary water source 
# recode for this using monthly visit data

#View(monthly[monthly$water_point1.wa_pt1==777&monthly$water_point1.wa_pt1_usebefore==2,c("water_point1.wa_pt1","water_point1.wa_source1","water_point1.wa_source1_other","water_point1.wa_tank1")])

monthly$h2o_collect1<-with(monthly, ifelse(water_point1.wa_pt1==1|water_point1.wa_pt1==2,1, #tap/pipe
                                ifelse(water_point1.wa_pt1==3|water_point1.wa_pt1==4,2, #handpump
                                ifelse(water_point1.wa_pt1==5|water_point1.wa_pt1==777,3,4)))) #well, all 777 were checked, and reported bucket in tank category 

monthly$h2o_source1<-with(monthly, ifelse(water_point1.wa_source1==1|water_point1.wa_source1==999,1, #WASA, only 999 was a handpump
                                           ifelse(water_point1.wa_source1==2|water_point1.wa_source1==3|water_point1.wa_source1==6,2, #Deep tube well
                                                  ifelse(water_point1.wa_source1==4|water_point1.wa_source1==5,3,0))))

monthly$h2o_tank1<-ifelse(monthly$water_point1.wa_tank1>0,1,0)

#### create script so that if the water collection point changes, subsequent visits change also
# 
# ifelse(monthly$first_visit==2&monthly$water_point1.wa_pt1_usebefore==1,
#        monthly$h2o_collect1==&monthly$h2o_source1==&monthly$h2o_tank1==,STAYS THE SAME ) 
# ifelse(monthly$first_visit==2&monthly$water_point1.wa_pt1_usebefore==0, all subsequent visits where monthly$water_point1.wa_pt1_usebefore==1 should match this)

#if monthly$water_point1.wa_pt1_usebefore==1, then use h2o_tank1, h2o_source1 and h2o_collect1 
     #from most previous date when monthly$water_point1.wa_pt1_usebefore==0

#View(monthly[monthly$water_point1.wa_source1==999&monthly$water_point1.wa_pt1_usebefore==2,c("water_point1.wa_pt1","water_point1.wa_source1","water_point1.wa_source1_other","water_point1.wa_tank1")])

monthly$water_access_group_d<-with(monthly, ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1==0,1, #tap, tank inside home
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,2, #tap, tank 0-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>=10,3, #tap, tank further than 10 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,7, #handpump, well 0-9 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>=10,8, #handpump, well further than 10 meters
                                                        ifelse(q14_recoded==2&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,9, #handpump, no well, 0-9 meters
                                                        ifelse(q14_recoded==2&q15_recoded==0&distance_to_source1>=10,10, #handpump, no well, further than 10 meters
                                                        ifelse(q14_recoded==3&distance_to_source1>0&distance_to_source1<10,11, #bucket, (well implied) 0-9 meters
                                                        ifelse(q14_recoded==3&distance_to_source1>=10,12, #bucket, (well implied) further than 10 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1==0,4, # tap, no tank, inside home
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,5, #tap, no tank, 1-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>=10,6,777))))))))))))) #tap, no tank, >10 meters

monthly$water_access_group_d2<-with(monthly, ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1==0,1, #tap, tank inside home
                                         ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,2, #tap, tank 0-9 meters
                                                ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>=10,3, #tap, tank further than 10 meters
                                                       ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,4, #handpump, well 0-9 meters
                                                              ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>=10,5, #handpump, well further than 10 meters
                                                                     ifelse(q14_recoded==2&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,6, #handpump, no well, 0-9 meters
                                                                            ifelse(q14_recoded==2&q15_recoded==0&distance_to_source1>=10,7, #handpump, no well, further than 10 meters
                                                                                   ifelse(q14_recoded==3&distance_to_source1>0&distance_to_source1<10,8, #bucket, (well implied) 0-9 meters
                                                                                          ifelse(q14_recoded==3&distance_to_source1>=10,9, #bucket, (well implied) further than 10 meters
                                                                                                 ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1==0,10, # tap, no tank, inside home
                                                                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,11, #tap, no tank, 1-9 meters
                                                                                                               ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>=10,12,777))))))))))))) #tap, no tank, >10 meters                                         
                                       
#preliminary analysis based on primary water source, means of water extraction and presence of tank
monthly$water_access_group<-with(monthly, ifelse(q14_recoded==1&q15_recoded==1,1, #tap, tank 
                                       ifelse(q14_recoded==1&q15_recoded==0,2, #tap, no tank 
                                              ifelse(q14_recoded==2&q15_recoded==1,3, #hand pump, tank 
                                                     ifelse(q14_recoded==2&q15_recoded==0,3, #handpump, no tank
                                                            ifelse(q14_recoded==3&q15_recoded==1,5, #bucket,tank
                                                                   ifelse(q14_recoded==3&q15_recoded==0,5,777))))))) #bucket, well 


#monthly[monthly$water_access_group==777,c("q14_recoded")]

                                                                          
#preliminary analysis based on primary water source, means of water extraction and presence of tank
monthly$water_access_group2<-with(monthly, ifelse(q14_recoded==1&q15_recoded==1,1, #tap, tank 
                                       ifelse(q14_recoded==1&q15_recoded==0,4, #tap, no tank 
                                              ifelse(q14_recoded==2&q15_recoded==1,2, #hand pump, tank 
                                                     ifelse(q14_recoded==2&q15_recoded==0,5, #handpump, no tank
                                                            ifelse(q14_recoded==3&q15_recoded==1,3, #bucket,tank
                                                                   ifelse(q14_recoded==3&q15_recoded==0,3,777))))))) #bucket, well 



#sub<-monthly[monthly$water_access_group==777,c("q14_recoded","q15_recoded","distance_to_source1")]

#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43
monthly$other_water_in.adult_bathe_in<-as.numeric(monthly$other_water_in.adult_bathe_in)
monthly$other_water_out.adult_bathe_out<-as.numeric(monthly$other_water_out.adult_bathe_out)
monthly$other_water_in.wash_plate_in<-as.numeric(monthly$other_water_in.wash_plate_in)
monthly$other_water_out.wash_plate_out<-as.numeric(monthly$other_water_out.wash_plate_out)
monthly$other_water_out.child_bathe_out<-as.numeric(monthly$other_water_out.child_bathe_out)
monthly$other_water_in.child_bathe_in<-as.numeric(monthly$other_water_in.child_bathe_in)
monthly$other_water_in.wash_clothes_in<-as.numeric(monthly$other_water_in.wash_clothes_in)
monthly$other_water_out.wash_clothes_out<-as.numeric(monthly$other_water_out.wash_clothes_out)

# 
# sub<-monthly[c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
#                               "cont3.cont3_size","cont3.cont3_times","cont4.cont4_size","cont4.cont4_times",
#                               "cont5.cont5_size","cont5.cont5_times","cont6.cont6_size","cont6.cont6_times","cont7.cont7_size",
#                               "cont7.cont7_times", "cont8.cont8_size","cont8.cont8_times","cont9.cont9_size",
#                               "cont9.cont9_times","cont10.cont10_size","cont10.cont10_times","cont11.cont11_size",
#                               "cont11.cont11_times","cont12.cont12_size","cont12.cont12_times","cont13.cont13_size",
#                               "cont13.cont13_times","cont14.cont14_size","cont14.cont14_times",                              
#                               "other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
#                               "other_water_in.wash_plate_in","other_water_out.wash_plate_out",
#                               "other_water_out.child_bathe_out","other_water_in.child_bathe_in",
#                               "other_water_in.wash_clothes_in","other_water_out.wash_clothes_out")]
#[is.na(monthly[c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
#                           "cont3.cont3_size","cont3.cont3_times","cont4.cont4_size","cont4.cont4_times",
#                           "cont5.cont5_size","cont5.cont5_times","cont6.cont6_size","cont6.cont6_times","cont7.cont7_size",
#                           "cont7.cont7_times", "cont8.cont8_size","cont8.cont8_times","cont9.cont9_size",
#                           "cont9.cont9_times","cont10.cont10_size","cont10.cont10_times","cont11.cont11_size",
#                           "cont11.cont11_times","cont12.cont12_size","cont12.cont12_times","cont13.cont13_size",
#                           "cont13.cont13_times","cont14.cont14_size","cont14.cont14_times",                              
#                           "other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
#                           "other_water_in.wash_plate_in","other_water_out.wash_plate_out",
#                           "other_water_out.child_bathe_out","other_water_in.child_bathe_in",
#                           "other_water_in.wash_clothes_in","other_water_out.wash_clothes_out")])]<-0
# 
# order(is.na(MonthlyAll$cont1.cont1_size))



#Checking data on activities done without a container
# monthly$bath_pc<-with(monthly,(other_water_in.adult_bathe_in+other_water_out.adult_bathe_out+other_water_in.child_bathe_in+other_water_out.child_bathe_out)/monthly$ppl)
# #check that all houses with child baths have children
# monthly$bath_child<-with(monthly,(other_water_in.child_bathe_in+other_water_out.child_bathe_out))
# write.csv2(monthly[monthly$bath_child>=1&monthly$children==0,c("new_per","new_per1_age","HHID", 
#           "date_visit", "FRA","ppl","other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
#           "other_water_in.child_bathe_in","other_water_out.child_bathe_out")],
#            file="Too many child baths.csv")

# monthly[monthly$HHID==246&monthly$new_per==2,c("new_per2_age")]
# 
# setwd("C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\Sent to bangladesh for clarification")
# write.csv2(monthly[monthly$bath_pc>3,c("HHID", "date_visit", "FRA","ppl","other_water_in.adult_bathe_in","other_water_out.adult_bathe_out","other_water_in.child_bathe_in","other_water_out.child_bathe_out")], 
#            file="Too many baths.csv")
# 
# 
# monthly$dishes<-with(monthly,(other_water_in.wash_plate_in+other_water_out.wash_plate_out))
# write.csv2(monthly[monthly$dishes>0,c("HHID", "date_visit", "FRA","ppl","other_water_in.wash_plate_in",
#                                              "other_water_out.wash_plate_out")],file="Washed dishes.csv")

#monthly[monthly$dishes>0,c("HHID")]

#monthly$clothes<-with(monthly,(other_water_in.wash_clothes_in+other_water_out.wash_clothes_out))
#write.csv2(monthly[monthly$clothes>5,c("HHID", "date_visit", "FRA","ppl","other_water_in.wash_clothes_in",
                                      #"other_water_out.wash_clothes_out")],file="Too many clothes.csv")

#range(monthly[monthly$clothes>0,c("clothes")])
  
#monthly[which(monthly$other_water_in.child_bathe_in==6&monthly$uniqueID=="269_2015-02-17"),"other_water_in.child_bathe_in"]<-0



monthly$daily_volume<-with(monthly, (cont1.cont1_size*cont1.cont1_times)+(cont2.cont2_size*cont2.cont2_times)+
                                                       (cont3.cont3_size*cont3.cont3_times)+(cont4.cont4_size*cont4.cont4_times)+(cont5.cont5_size*cont5.cont5_times)
                                                     +(cont6.cont6_size*cont6.cont6_times)+(cont7.cont7_size*cont7.cont7_times)+(cont8.cont8_size*cont8.cont8_times)
                                                     +(cont9.cont9_size*cont9.cont9_times)+(cont10.cont10_size*cont10.cont10_times)+(cont11.cont11_size*cont11.cont11_times)
                                                     +(cont12.cont12_size*cont12.cont12_times)+(cont13.cont13_size*cont13.cont13_times)+(cont14.cont14_size*cont14.cont14_times)
                                                     +((other_water_in.adult_bathe_in+other_water_out.adult_bathe_out)*37)  #will probably change once more detailed information is received from Rebeca
                                                     +((other_water_out.child_bathe_out+other_water_in.child_bathe_in)*14))
                                                                                   
monthly$daily_h2o_percapita<- monthly$daily_volume/monthly$ppl

mean(monthly$daily_h2o_percapita)
range(monthly$daily_h2o_percapita)


#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43
#create H20 per capita variable



#check values
View(monthly[monthly$daily_h2o_percapita>200, c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
                                          "cont3.cont3_size","cont3.cont3_times","cont4.cont4_size","cont4.cont4_times",
                                          "cont5.cont5_size","cont5.cont5_times","cont6.cont6_size","cont6.cont6_times","cont7.cont7_size", 
                                          "cont7.cont7_times", "cont8.cont8_size","cont8.cont8_times","cont9.cont9_size", 
                                          "cont9.cont9_times","cont10.cont10_size","cont10.cont10_times","cont11.cont11_size",
                                          "cont11.cont11_times","cont12.cont12_size","cont12.cont12_times","cont13.cont13_size",
                                                                     "cont13.cont13_times","cont14.cont14_size","cont14.cont14_times",                              
                                                                     "other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
                                                                     "other_water_out.child_bathe_out","other_water_in.child_bathe_in")])   

#quintiles based on daily H20 consumption per capita
monthly$h2o_percap_quintile<-as.integer(cut(monthly$daily_h2o_percapita,
                                            quantile(monthly$daily_h2o_percapita,probs=0:5/5,include.lowest=TRUE)))


waterusebytank<-t.test(monthly$h2o_percap_quintile~monthly$q15_recoded) #is there a tank present
summary(waterusebytank)         
waterusebytap<-anova(monthly$h2o_percap_quintile~monthly$q14_recoded) #tap vs. handpump vs. bucket
summary(waterusebytap)
waterusebysource<-anova(monthly$h2o_percap_quintile~monthly$q14a_recoded) #WASA, DTW, STW
summary(waterusebysource)


boxplot(daily_h2o_percapita~month, data=monthly)

#summary(monthly$daily_h2o_percapita)

# Asset calculation -------------------------------------------------------
#shared facilities (water q17, kitchen q31, latrines q35) 0=all shared, 1=2of3 shared, 2= 1of3 shared
monthly$shared_facilities<- with(monthly, ifelse(q17==1& q31 >=1 & q35==1, 0, 
                                                 ifelse(q17==0& q31 >=1 & q35==1, 1,
                                                        ifelse(q17==1& q31 >=1 & q35==0, 1,
                                                               ifelse(q17==1& q31 ==0 & q35==1, 1,
                                                                      ifelse(q17==0& q31 >=1 & q35==0, 2,
                                                                             ifelse(q17==0& q31 ==0 & q35==1, 2,
                                                                                    ifelse(q17==1& q31 ==0 & q35==0, 2, NA))))))))
#table(monthly$shared_facilities)

#### Assets continued: ownership of items
# in q9_20_oth1, q9_20_oth2, q9_20_oth3 meatshelf (cupboard), dressing table (dresser), and sofa should earn 1 point, rickshaw should earn 2, 
monthly$q9_20_oth1<-as.character(monthly$q9_20_oth1)
monthly$q9_20_oth2 <-as.character(monthly$q9_20_oth2)
monthly$q9_20_oth3 <-as.character(monthly$q9_20_oth3)

#recode spelling errors, everything that starts with M becomes MEAT SHELF, D becomes DRESSING TABLE
#grep("^C", monthly$q9_20_oth1, value=T) #to search for resopnses by start letter

monthly[grep("^M", monthly$q9_20_oth1), "q9_20_oth1"] <- as.character("MEAT SHELF")
monthly[grep("^DRE", monthly$q9_20_oth1), "q9_20_oth1"] <- as.character("DRESSING TABLE")

monthly[grep("^IRON", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
monthly[grep("^M", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
monthly[grep("^DRE", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("DRESSING TABLE")

monthly[grep("^M", monthly$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")
monthly[grep("^IRON", monthly$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")

monthly$q9_20_oth1p <- with(monthly, ifelse(q9_20_oth1=="COMPUTER",3,
                                            ifelse(q9_20_oth1=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth1=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth1=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth1=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth1=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth1=="SOFA",1,
                                                                                      ifelse(q9_20_oth1=="SUFA",1,
                                                                                             ifelse(q9_20_oth1=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth1=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))

monthly$q9_20_oth2p <- with(monthly, ifelse(q9_20_oth2=="COMPUTER",3,
                                            ifelse(q9_20_oth2=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth2=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth2=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth2=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth2=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth2=="SOFA",1,
                                                                                      ifelse(q9_20_oth2=="SUFA",1,
                                                                                             ifelse(q9_20_oth2=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth2=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))

monthly$q9_20_oth3p <- with(monthly, ifelse(q9_20_oth3=="COMPUTER",3,
                                            ifelse(q9_20_oth3=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth3=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth3=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth3=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth3=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth3=="SOFA",1,
                                                                                      ifelse(q9_20_oth3=="SUFA",1,
                                                                                             ifelse(q9_20_oth3=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth3=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))
monthly$q9_other_sum<-with(monthly, q9_20_oth1p+q9_20_oth2p+q9_20_oth3p)

#count assets
monthly$asset_score<-NA
monthly$asset_score<- (monthly$q9_other_sum + 
                         as.numeric(monthly$q9_2) + as.numeric(monthly$q9_3) +
                         as.numeric(monthly$q9_5) + as.numeric(monthly$q9_7) + 
                         as.numeric(monthly$q9_8) + as.numeric(monthly$q9_9) + 
                         as.numeric(monthly$q9_16) + as.numeric(monthly$q9_18)+ 
                         ((as.numeric(monthly$q9_1) + as.numeric(monthly$q9_6) +
                             as.numeric(monthly$q9_10) + (as.numeric(monthly$q9_11) + 
                                                            as.numeric(monthly$q9_12) + as.numeric(monthly$q9_14) + 
                                                            as.numeric(monthly$q9_15 + as.numeric(monthly$q9_17))*2) + 
                             ((as.numeric(monthly$q9_13) + as.numeric(monthly$q9_19) + 
                                 monthly$shared_facilities)*3))))

#create column with asset quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
monthly$asset_score[is.na(monthly$asset_score)]<-0

monthly$asset_quintile<-as.integer(cut(monthly$asset_score,
                                            quantile(monthly$asset_score,probs=0:5/5,include.lowest=TRUE)))


# Water Source ------------------------------------------------------------
#q14a_recoded WASA=1; deep tube well/submersible = 2 ; well/shallow tube well = 3







# Calculate time that water was available during the previous 24 hours --------
#Add 6 hours to get into correct time zone
hrs <- function(u) {
  x <- u * 3600
  return(x)
}
monthly$water_flow1_end2<-strptime(monthly$water_point1.wa_flow1.wa_time1.aE,"%H:%M:%S")+hrs(6)
monthly$water_flow1_start2<-strptime(monthly$water_point1.wa_flow1.wa_time1.aS,"%H:%M:%S")-hrs(18)
monthly$water_flow1_end1<-strptime(monthly$water_point1.wa_flow1.wa_time1.aE,"%H:%M:%S")
monthly$water_flow1_start1<-strptime(monthly$water_point1.wa_flow1.wa_time1.aS,"%H:%M:%S")

monthly$flow1<-difftime(monthly$water_flow1_end1,monthly$water_flow1_start1, units=("hours"))
monthly$flow2<-difftime(monthly$water_flow1_end2,monthly$water_flow1_start2, units=("hours"))

View(monthly[,c("water_flow1_end1", "water_flow1_start1","water_flow1_end2", "water_flow1_start2")])

##############following does not work#################
monthly$water_flow1_end1$hour<-with(monthly, times(water_flow1_end$hour+6))

monthly$water_flow1_end<-as.POSIXlt(monthly$water_point1.wa_flow1.wa_time1.aE, format="%H:%M:%S")$hour

View(monthly[,c("water_point1.wa_avail1","water_point1.wa_flow1.wa_time1.aS",
               "water_point1.wa_flow1.wa_time1.aE", "water_point1.wa_flow1.wa_time1a",  
               "water_point1.wa_flow1.wa_time1.bS", "water_point1.wa_flow1.wa_time1.bE",
               "water_point1.wa_flow1.wa_time1b",   "water_point1.wa_flow1.wa_time1.cS",
               "water_point1.wa_flow1.wa_time1.cE", "water_point1.wa_flow1.wa_time1c","flow1")])

monthly$water_end1<-with(monthly,ifelse(water_point1.wa_flow1.wa_time1.aE<water_point1.wa_flow1.wa_time1.aS,water_point1.wa_flow1.wa_time1.aE+24:00:00,water_point1.wa_flow1.wa_time1.aE))

#is there 24 hours water access
monthly$allday_h2o <-with(monthly, ifelse(water_point1.wa_flow1.wa_time1.aE=="17:59:00"&
                                            water_point1.wa_flow1.wa_time1.aS=="18:00:00", 1, 0))

#how far away is the collection point from the front door
monthly$distance<-with(monthly, ifelse(distance_to_source1==0,1,
                                       ifelse(distance_to_source1>0&distance_to_source1<=10,2,
                                              ifelse(distance_to_source1>10&distance_to_source1<=20,3,4))))


save(monthly, file = data.out.path)



# Relationships between variables -------------------------------------------------------


#plots of months and water consumption
monthly$month<-formatC(monthly$month,width=2,format='d', flag = 0)

monthly$year.month<-as.numeric(with(monthly, ifelse(month>8,paste("14.",month,sep=""),paste("15.",month,sep=""))))



#############
boxplot(daily_h2o_percapita~month,data = sub)


#look for relationships between water consumption and different variables
# lmq15<-lm(daily_h2o_percapita~q15_recoded,monthly) # p=0.2319
# lmq14<-lm(daily_h2o_percapita~q14_recoded,monthly) # p=0.381
# lmq14a<-lm(daily_h2o_percapita~q14a_recoded,monthly)#p=0.5836
monthly$distance<-with(monthly, ifelse(distance_to_source1==0,1,
                                   ifelse(distance_to_source1>0&distance_to_source1<=10,2,
                                          ifelse(distance_to_source1>10&distance_to_source1<=20,3,4))))

# watergroupd<-anova(daily_h2o_percapita~water_access_group_d,monthly)#p=0.3102
# watergroupd2<-anova(daily_h2o_percapita~water_access_group_d2,monthly) #p=0.2393
# 
# watergroup1<-anova(daily_h2o_percapita~water_access_group,monthly) #p=0.03695 0.2928
# watergroup2<-anova(daily_h2o_percapita~water_access_group2,monthly)#p=0.3109

#lm_dist<-anova(daily_h2o_percapita~h20_distance_coded,monthly)# p=0.8836
monthly$h2o_inside_home<-with(monthly, ifelse(distance_to_source1==0,1,2))
#lm_h2oinsidehome<-anova(daily_h2o_percapita~h2o_inside_home,monthly)# p=0.4038
summary(lm_watergroup1)

with(monthly,table(q14a==2~q15_recoded))
summary(monthly[monthly$q14a_recoded==2&monthly$allday_h2o==1&monthly$q15_recoded==1,]) #100, 82 of which have tanks
summary(monthly[monthly$q14a_recoded==2&monthly$allday_h2o==0&monthly$q15_recoded==1,]) #762, 645 of which have tanks
summary(monthly[monthly$q14a_recoded==2&monthly$q15_recoded==1,])# 727

####use boxplots here#### boxplot(frequency ~ attitude*gender,
##col=c("white","lightgray"),politeness)
# Linear models -----------------------------------------------------------

#model 1, 1= tap,tank 2= tap,no tank, 3=handpump, tank, 4= handpump no tank, 5= bucket tank
sub<-monthly[monthly$daily_h2o_percapita<150&monthly$daily_h2o_percapita>20,] #get rid of outliers





model1=lmer(daily_h2o_percapita ~ season  +  distance + allday_h2o+ q15_recoded + q14_recoded + asset_quintile
            + (1|slno.1) +(1|listing), data=sub)
summary(model1)

model.null=lmer(daily_h2o_percapita ~ allday_h2o +  distance + h2o_tank1 + h2o_collect1 + asset_quintile
                + (1|slno.1) +(1|listing), data=sub)
summary(model1)

anova(model1,model.null)

# model2=lmer(daily_h2o_percapita ~ season + allday_h2o + (1+distance|slno.1) (1+q15_recoded|slno.1) + (1+q14a_recoded|slno.1) 
#             +(1+asset_quintile|slno.1), data=sub1)
# 
# 
# model2.null=lmer(daily_h2o_percapita ~  allday_h2o + (1+distance|slno.1) (1+q15_recoded|slno.1) + 
#                    (1+q14a_recoded|slno.1)  +(1+asset_quintile|slno.1), data=sub1)
# 


