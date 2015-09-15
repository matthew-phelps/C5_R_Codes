#baseline descriptives

library(chron)
library(lme4)

# Load data ---------------------------------------------------------------
ifelse(grepl("zrc340", getwd()), 
       Q11.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\Q11_all.Rdata",
       Q11.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Q11_all.Rdata")
ifelse(grepl("zrc340", getwd()), 
       m4.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       m4.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")

load(m4.path)
load(Q11.path)

# Household size at baseline----------------------------------------------------------

monthly$children<-monthly$children_U5+monthly$children_5_17
monthly$total_hh_members<- monthly$children + monthly$adult

# Water access groups -----------------------------------------------------
#
#q14_recoded, 1 = pipe/tap, 2= hand pump, 3= well with bucket
#q14a_recoded, WASA=1; deep tube well/submersible =2,3 <-2 ; well/shallow tube well = 4,5 <-3;
#q15_recoded, is there a tank? 1=yes, 0=no
#use_bucket, is a bucket needed to withdraw water? 1=yes, 0=no
#m4$distance_to_source1, anything over 20 meters =21
#m4$daily_volume<-as.numeric(m4$cont1.cont1_size)*(as.numeric(m4$cont1.cont1_times))
#for primary water source 

m4$water_access_group<-with(m4, ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1==0,1, #tap, tank inside home
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,2, #tap, tank 0-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>=10,3, #tap, tank further than 10 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,4, #handpump, well 0-9 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>=10,5, #handpump, well further than 10 meters
                                                        ifelse(q14_recoded==3&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,6, #bucket, well 0-9 meters
                                                        ifelse(q14_recoded==3&q15_recoded==1&distance_to_source1>=10,7, #bucket, well further than 10 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1==0,8, # tap, no tank, inside home
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,9, #tap, no tank, 1-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>=10,10,777))))))))))) #tap, no tank, >10 meters

#preliminary analysis based on primary water source
m4$water_access_group<-with(m4, ifelse(q14_recoded==1&q15_recoded==1,1, #tap, tank 
                                       ifelse(q14_recoded==1&q15_recoded==0,2, #tap, no tank 
                                              ifelse(q14_recoded==2&q15_recoded==1,3, #hand pump, tank 
                                                     ifelse(q14_recoded==2&q15_recoded==0,4, #handpump, no tank
                                                            ifelse(q14_recoded==3&q15_recoded==1,5, #bucket,tank
                                                                   ifelse(q14_recoded==3&q15_recoded==0,5,777))))))) #bucket, well 
                                                                          
#preliminary analysis based on primary water source
m4$water_access_group2<-with(m4, ifelse(q14_recoded==1&q15_recoded==1,1, #tap, tank 
                                       ifelse(q14_recoded==1&q15_recoded==0,4, #tap, no tank 
                                              ifelse(q14_recoded==2&q15_recoded==1,2, #hand pump, tank 
                                                     ifelse(q14_recoded==2&q15_recoded==0,5, #handpump, no tank
                                                            ifelse(q14_recoded==3&q15_recoded==1,3, #bucket,tank
                                                                   ifelse(q14_recoded==3&q15_recoded==0,3,777))))))) #bucket, well 


#sub<-m4[m4$water_access_group==777,c("q14_recoded","q15_recoded","distance_to_source1")]

#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43
m4$other_water_in.adult_bathe_in<-as.numeric(m4$other_water_in.adult_bathe_in)
m4$other_water_out.adult_bathe_out<-as.numeric(m4$other_water_out.adult_bathe_out)
m4$other_water_in.wash_plate_in<-as.numeric(m4$other_water_in.wash_plate_in)
m4$other_water_out.wash_plate_out<-as.numeric(m4$other_water_out.wash_plate_out)
m4$other_water_out.child_bathe_out<-as.numeric(m4$other_water_out.child_bathe_out)
m4$other_water_in.child_bathe_in<-as.numeric(m4$other_water_in.child_bathe_in)
m4$other_water_in.wash_clothes_in<-as.numeric(m4$other_water_in.wash_clothes_in)
m4$other_water_out.wash_clothes_out<-as.numeric(m4$other_water_out.wash_clothes_out)

#subset only data from monthly visits
monthly<-m4[which(!(is.na(m4$FRA))),]
monthly<-as.data.frame(monthly)
monthly[is.na(monthly)]<-0

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
#                               "other_water_in.wash_clothes_in","other_water_out.wash_clothes_out")][is.na(m4[c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
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
monthly$daily_volume<-with(monthly, (cont1.cont1_size*cont1.cont1_times)+(cont2.cont2_size*cont2.cont2_times)+
                                                       (cont3.cont3_size*cont3.cont3_times)+(cont4.cont4_size*cont4.cont4_times)+(cont5.cont5_size*cont5.cont5_times)
                                                     +(cont6.cont6_size*cont6.cont6_times)+(cont7.cont7_size*cont7.cont7_times)+(cont8.cont8_size*cont8.cont8_times)
                                                     +(cont9.cont9_size*cont9.cont9_times)+(cont10.cont10_size*cont10.cont10_times)+(cont11.cont11_size*cont11.cont11_times)
                                                     +(cont12.cont12_size*cont12.cont12_times)+(cont13.cont13_size*cont13.cont13_times)+(cont14.cont14_size*cont14.cont14_times)
                                                     +((other_water_in.adult_bathe_in+other_water_out.adult_bathe_out)*37) + #will probably change once more detailed information is received from Rebeca
                                                       ((other_water_in.wash_plate_in+other_water_out.wash_plate_out)*25) +
                                                       ((other_water_out.child_bathe_out+other_water_in.child_bathe_in)*14)+
                                                       ((other_water_in.wash_clothes_in+other_water_out.wash_clothes_out)*43))                              
                              

#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43
#create H20 per capita variable

monthly$ppl<-ifelse(monthly$ppl==0,monthly$total_HH_members,monthly$ppl)
monthly$daily_h2o_percapita<-with(monthly, daily_volume/ppl)

#quintiles based on daily H20 consumption per capita
monthly$h2o_percap_quintile<-as.integer(cut(monthly$daily_h2o_percapita,
                                            quantile(monthly$daily_h2o_percapita,probs=0:5/5,include.lowest=TRUE)))

waterusebytank<-lm(monthly$h2o_percap_quintile~monthly$q15_recoded) #is there a tank present
summary(waterusebytank)         
waterusebytap<-lm(monthly$h2o_percap_quintile~monthly$q14_recoded) #tap vs. handpump vs. bucket
summary(waterusebytap)
waterusebysource<-lm(monthly$h2o_percap_quintile~monthly$q14a_recoded) #WASA, DTW, STW
summary(waterusebysource)


#summary(monthly$daily_h2o_percapita)



# Water Source ------------------------------------------------------------
#q14a_recoded WASA=1; deep tube well/submersible = 2 ; well/shallow tube well = 3


# Household structure -----------------------------------------------------
#q10 nuclear =1, multiple families =2, unrelated persons = 3, nuclear family with 1+ unrelated = 4, other=777
#table(m4$q10oth)
monthly[monthly$q10oth=="SINGLE","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE.","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE (MESS)","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE PERSON","q10"]<-3 #single is mess hall, same as unrelated people
monthly[grep("^SINGLE MEMBER,", monthly$q10oth),"q10"]<-4 #this is one single person living with nuclear family
monthly[grep("^ONE", monthly$q10oth),"q10"]<-4 #this is one single person living with nuclear family
monthly[grep("^ONLY", monthly$q10oth),"q10"]<-1 #two sisters living together
#monthly$q10oth[monthly$q10==777]

monthly$nuclear_family<- with(monthly, ifelse(q10==1|q10==2|q10==4,1,0)) #is it one or more nuclear families (1) or primarily unrelated people (0)?

# Occupation --------------------------------------------------------------
###q11_6 employment: unemployed/student/retired=6, Un-skilled labor = 1, skilled labor = 2, Garments =3, salaried job = 4, spiritual healer=5, other = 777

table(Q11_all$q11_6)
table(Q11_all$q11_6)
Q11_all$jobs<-with(Q11_all, ifelse(q11_6==0,NA,
                                   ifelse(q11_6==1|q11_6==3|q11_6==4|q11_6==9|q11_6==11|q11_6==33,1,
                                   ifelse(q11_6==2|q11_6==29|q11_6==30|q11_6==31|q11_6==32,6,
                                   ifelse(q11_6==5|q11_6==16|q11_6==17|q11_6==18|q11_6==19,4,
                                   ifelse(q11_6==6,3,
                                   ifelse(q11_6==7|q11_6==8|q11_6==15|q11_6==20|q11_6==21|q11_6==24,2,
                                   ifelse(q11_6==26,5,q11_6))))))))
sub<-Q11_all[Q11_all$q11_4>=18,] #look only at adults
table(sub$jobs)


# Household monthly income ------------------------------------------------
#monthly income = average monthly household income + monthly remittances received - monthly remittances sent + annual remittances received/12 - annual remittances sent/12 - monthly loan payment
monthly$Monthly_income<- monthly$q12 + monthly$q12a2 - monthly$q12a1 + 
  (monthly$q12a3/12)-(monthly$q12a4/12)- monthly$q12d

monthly$monthly_income_percapita<-monthly$Monthly_income/(monthly$ppl)

# View(monthly$monthly_income_percapita)
#create column with income quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
monthly$pc_income_quintile<-as.integer(cut(monthly$monthly_income_percapita, quantile(monthly$Monthly_income_percapita, 
    
                                                                                                                                                                                        probs=0:5/5, include.lowest=TRUE)))
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

table(monthly$q9_20_oth1)
#count assets
monthly$asset_score<-NA
monthly$asset_score<- (monthly$q9_other_sum + 
                             as.numeric(monthly$q9_2) + as.numeric(monthly$q9_3) +
                             as.numeric(monthly$q9_5) +  
                             as.numeric(monthly$q9_7) + as.numeric(monthly$q9_8) +
                             as.numeric(monthly$q9_9) + 
                             as.numeric(monthly$q9_16) + as.numeric(monthly$q9_18)+ 
                             ((as.numeric(monthly$q9_1) + as.numeric(monthly$q9_6) +
                                 as.numeric(monthly$q9_10) + (as.numeric(monthly$q9_11) + 
                                                                    as.numeric(monthly$q9_12) + as.numeric(monthly$q9_14) + 
                                                                    as.numeric(monthly$q9_15 + as.numeric(monthly$q9_17))*2) + 
                                 ((as.numeric(monthly$q9_13) + as.numeric(monthly$q9_19) + 
                                     monthly$shared_facilities)*3))))

#create column with asset quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
monthly$asset_score[is.na(monthly$asset_score)]<-0

monthly$asset_quintile<-as.integer(cut(monthly$asset_score, quantile(monthly$asset_score, 
                                                                             probs=0:5/5, include.lowest=TRUE)))

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

# Linear models -----------------------------------------------------------


#first create month variable
monthly$date_visit_character<-as.character(monthly$date_visit)
temp<-strsplit(monthly$date_visit_character, "-")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("year", "month", "day")
monthly<- cbind(monthly,df)

lm(monthly$daily_h2o_percapita~monthly$water_access_group)

#model 1, 1= tap,tank 2= tap,no tank, 3=handpump, tank, 4= handpump no tank, 5= bucket tank
model1=lmer(daily_h2o_percapita ~ month*water_access_group + (1|uniqueID) +(1|asset_score), data=monthly)
summary(model1)
model1.null=lmer(daily_h2o_percapita ~ water_access_group + (1|uniqueID)+(1|asset_score), data=monthly)
summary(model.null)

#model 2, 1= tap,tank 2= handpump, tank, 3= bucket tank 4= tap,no tank, 5= handpump no tank 
model2=lmer(daily_h2o_percapita ~ month*water_access_group2 + (1|uniqueID), data=monthly)
#summary(model2)
model2.null=lmer(daily_h2o_percapita ~ water_access_group2 + (1|uniqueID), data=monthly)
#summary(model2.null)

model24.null=lmer(daily_h2o_percapita ~ allday_h2o + water_access_group2 + (1|uniqueID), data=monthly)
model24=lmer(daily_h2o_percapita ~ month*allday_h2o + water_access_group2 + (1|uniqueID), data=monthly)

model24.null=lmer(daily_h2o_percapita ~ allday_h2o + water_access_group + (1|uniqueID), data=monthly)
model24=lmer(daily_h2o_percapita ~ month*allday_h2o + water_access_group + (1|uniqueID), data=monthly)

modelq2.null=lmer(h2o_percap_quintile ~ allday_h2o + water_access_group2 + (1|uniqueID), data=monthly)
modelq2=lmer(h2o_percap_quintile ~ month*allday_h2o + water_access_group2 + (1|uniqueID), data=monthly)

modelq.null=lmer(h2o_percap_quintile ~ allday_h2o + water_access_group + (1|uniqueID), data=monthly)
modelq=lmer(h2o_percap_quintile ~ month*allday_h2o + water_access_group + (1|uniqueID), data=monthly)


#anova analysis to look for statistical significance 
anova(model1,model1.null)
anova(model2,model2.null)
anova(model24,model24.null)
anova(modelq,modelq.null)
anova(modelq2,modelq2.null)


