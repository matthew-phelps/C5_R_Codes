#baseline descriptives



# Load data ---------------------------------------------------------------
#run B1 first


# Water access groups -----------------------------------------------------
#
#q14_recoded, 1 = pipe/tap, 2= hand pump, 3= well with bucket
#q14a_recoded, WASA=1; deep tube well/submersible =2,3 <-2 ; well/shallow tube well = 4,5 <-3;
#q15_recoded, is there a tank? 1=yes, 0=no
#use_bucket, is a bucket needed to withdraw water? 1=yes, 0=no
#baselineAll$distance_to_source1, anything over 20 meters =21
#MonthlyAll$daily_volume<-as.numeric(MonthlyAll$cont1.cont1_size)*(as.numeric(MonthlyAll$cont1.cont1_times))
#for primary water source 
baselineAll$water_access_group<-with(baselineAll, ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1==0,1, #tap, tank inside home
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,2, #tap, tank 0-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==1&distance_to_source1>10,3, #tap, tank further than 10 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,4, #handpump, well 0-9 meters
                                                        ifelse(q14_recoded==2&q15_recoded==1&distance_to_source1>10,5, #handpump, well further than 10 meters
                                                        ifelse(q14_recoded==3&q15_recoded==1&distance_to_source1>0&distance_to_source1<10,6, #bucket, well 0-9 meters
                                                        ifelse(q14_recoded==3&q15_recoded==1&distance_to_source1>10,7, #bucket, well further than 10 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1==0,8, # tap, no tank, inside home
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>0&distance_to_source1<10,9, #tap, no tank, 1-9 meters
                                                        ifelse(q14_recoded==1&q15_recoded==0&distance_to_source1>10,10,777))))))))))) #tap, no tank, >10 meters

#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43
MonthlyAll$other_water_in.adult_bathe_in<-as.numeric(MonthlyAll$other_water_in.adult_bathe_in)

x[c("a", "b")][is.na(x[c("a", "b")])] <- 0

MonthlyAll[c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
                              "cont3.cont3_size","cont3.cont3_times","cont4.cont4_size","cont4.cont4_times",
                              "cont5.cont5_size","cont5.cont5_times","cont6.cont6_size","cont6.cont6_times","cont7.cont7_size",
                              "cont7.cont7_times", "cont8.cont8_size","cont8.cont8_times","cont9.cont9_size",
                              "cont9.cont9_times","cont10.cont10_size","cont10.cont10_times","cont11.cont11_size",
                              "cont11.cont11_times","cont12.cont12_size","cont12.cont12_times","cont13.cont13_size",
                              "cont13.cont13_times","cont14.cont14_size","cont14.cont14_times",                              
                              "other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
                              "other_water_in.wash_plate_in","other_water_out.wash_plate_out",
                              "other_water_out.child_bathe_out","other_water_in.child_bathe_in",
                              "other_water_in.wash_clothes_in","other_water_out.wash_clothes_out")][is.na(MonthlyAll[c("cont1.cont1_size","cont1.cont1_times","cont2.cont2_size","cont2.cont2_times",                           
                          "cont3.cont3_size","cont3.cont3_times","cont4.cont4_size","cont4.cont4_times",
                          "cont5.cont5_size","cont5.cont5_times","cont6.cont6_size","cont6.cont6_times","cont7.cont7_size",
                          "cont7.cont7_times", "cont8.cont8_size","cont8.cont8_times","cont9.cont9_size",
                          "cont9.cont9_times","cont10.cont10_size","cont10.cont10_times","cont11.cont11_size",
                          "cont11.cont11_times","cont12.cont12_size","cont12.cont12_times","cont13.cont13_size",
                          "cont13.cont13_times","cont14.cont14_size","cont14.cont14_times",                              
                          "other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
                          "other_water_in.wash_plate_in","other_water_out.wash_plate_out",
                          "other_water_out.child_bathe_out","other_water_in.child_bathe_in",
                          "other_water_in.wash_clothes_in","other_water_out.wash_clothes_out")])]<-0


MonthlyAll$daily_volume<-with(MonthlyAll, (cont1.cont1_size*cont1.cont1_times)+(cont2.cont2_size*cont2.cont2_times)+
                                                       (cont3.cont3_size*cont3.cont3_times)+(cont4.cont4_size*cont4.cont4_times)+(cont5.cont5_size*cont5.cont5_times)
                                                     +(cont6.cont6_size*cont6.cont6_times)+(cont7.cont7_size*cont7.cont7_times)+(cont8.cont8_size*cont8.cont8_times)
                                                     +(cont9.cont9_size*cont9.cont9_times)+(cont10.cont10_size*cont10.cont10_times)+(cont11.cont11_size*cont11.cont11_times)
                                                     +(cont12.cont12_size*cont12.cont12_times)+(cont13.cont13_size*cont13.cont13_times)+(cont14.cont14_size*cont14.cont14_times)
                                                     +((other_water_in.adult_bathe_in+other_water_out.adult_bathe_out)*37) + #will probably change once more detailed information is received from Rebeca
                                                       ((other_water_in.wash_plate_in+other_water_out.wash_plate_out)*25) +
                                                       ((other_water_out.child_bathe_out+other_water_in.child_bathe_in)*14)+
                                                       ((other_water_in.wash_clothes_in+other_water_out.wash_clothes_out)*43))                              
                              

#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43

# Household size ----------------------------------------------------------

baselineAll$child_5_17 
baselineAll$adult
baselineAll$total_members<- baselineAll$child_5_17 + baselineAll$adult



# Water Source ------------------------------------------------------------
#q14a_recoded WASA=1; deep tube well/submersible = 2 ; well/shallow tube well = 3


# Household structure -----------------------------------------------------
#q10 nuclear =1, multiple families =2, unrelated persons = 3, nuclear family with 1+ unrelated = 4, other=777
#table(baselineAll$q10oth)
baselineAll[baselineAll$q10oth=="SINGLE","q10"]<-3 #single is mess hall, same as unrelated people
baselineAll[baselineAll$q10oth=="SINGLE.","q10"]<-3 #single is mess hall, same as unrelated people
baselineAll[baselineAll$q10oth=="SINGLE (MESS)","q10"]<-3 #single is mess hall, same as unrelated people
baselineAll[baselineAll$q10oth=="SINGLE PERSON","q10"]<-3 #single is mess hall, same as unrelated people
baselineAll[grep("^SINGLE MEMBER,", baselineAll$q10oth),"q10"]<-4 #this is one single person living with nuclear family
baselineAll[grep("^ONE", baselineAll$q10oth),"q10"]<-4 #this is one single person living with nuclear family
baselineAll[grep("^ONLY", baselineAll$q10oth),"q10"]<-1 #two sisters living together
#baselineAll$q10oth[baselineAll$q10==777]

baselineAll$nuclear_family<- with(baselineAll, ifelse(q10==1|q10==2|q10==4,1,0)) #is it one or more nuclear families (1) or primarily unrelated people (0)?

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
baselineAll$Monthly_income<- baselineAll$q12 + baselineAll$q12a2 - baselineAll$q12a1 + 
  (baselineAll$q12a3/12)-(baselineAll$q12a4/12)- baselineAll$q12d

baselineAll$monthly_income_percapita<-baselineAll$Monthly_income/(baselineAll$total_HH_members)

# View(baselineAll$monthly_income_percapita)
#create column with income quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
baselineAll$pc_income_quintile<-as.integer(cut(baselineAll$monthly_income_percapita, quantile(baselineAll$Monthly_income_percapita, 
    
                                                                                                                                                                                        probs=0:5/5, include.lowest=TRUE)))
# Asset calculation -------------------------------------------------------
#shared facilities (water q17, kitchen q31, latrines q35) 0=all shared, 1=2of3 shared, 2= 1of3 shared
baselineAll$shared_facilities<- with(baselineAll, ifelse(q17==1& q31 >=1 & q35==1, 0, 
                                                  ifelse(q17==0& q31 >=1 & q35==1, 1,
                                                    ifelse(q17==1& q31 >=1 & q35==0, 1,
                                                    ifelse(q17==1& q31 ==0 & q35==1, 1,
                                                    ifelse(q17==0& q31 >=1 & q35==0, 2,
                                                    ifelse(q17==0& q31 ==0 & q35==1, 2,
                                                    ifelse(q17==1& q31 ==0 & q35==0, 2, NA))))))))
#table(baselineAll$shared_facilities)

#### Assets continued: ownership of items
# in q9_20_oth1, q9_20_oth2, q9_20_oth3 meatshelf (cupboard), dressing table (dresser), and sofa should earn 1 point, rickshaw should earn 2, 
baselineAll$q9_20_oth1<-as.character(baselineAll$q9_20_oth1)
baselineAll$q9_20_oth2 <-as.character(baselineAll$q9_20_oth2)
baselineAll$q9_20_oth3 <-as.character(baselineAll$q9_20_oth3)

#recode spelling errors, everything that starts with M becomes MEAT SHELF, D becomes DRESSING TABLE
#grep("^C", baselineAll$q9_20_oth1, value=T) #to search for resopnses by start letter

baselineAll[grep("^M", baselineAll$q9_20_oth1), "q9_20_oth1"] <- as.character("MEAT SHELF")
baselineAll[grep("^DRE", baselineAll$q9_20_oth1), "q9_20_oth1"] <- as.character("DRESSING TABLE")

baselineAll[grep("^IRON", baselineAll$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
baselineAll[grep("^M", baselineAll$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
baselineAll[grep("^DRE", baselineAll$q9_20_oth2), "q9_20_oth2"] <- as.character("DRESSING TABLE")

baselineAll[grep("^M", baselineAll$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")
baselineAll[grep("^IRON", baselineAll$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")

baselineAll$q9_20_oth1p <- with(baselineAll, ifelse(q9_20_oth1=="COMPUTER",3,
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

baselineAll$q9_20_oth2p <- with(baselineAll, ifelse(q9_20_oth2=="COMPUTER",3,
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

baselineAll$q9_20_oth3p <- with(baselineAll, ifelse(q9_20_oth3=="COMPUTER",3,
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
baselineAll$q9_other_sum<-with(baselineAll, q9_20_oth1p+q9_20_oth2p+q9_20_oth3p)

table(baselineAll$q9_20_oth1)
#count assets
baselineAll$asset_score<-NA
baselineAll$asset_score<- (baselineAll$q9_other_sum + 
                             as.numeric(baselineAll$q9_2) + as.numeric(baselineAll$q9_3) +
                             as.numeric(baselineAll$q9_5) +  
                             as.numeric(baselineAll$q9_7) + as.numeric(baselineAll$q9_8) +
                             as.numeric(baselineAll$q9_9) + 
                             as.numeric(baselineAll$q9_16) + as.numeric(baselineAll$q9_18)+ 
                             ((as.numeric(baselineAll$q9_1) + as.numeric(baselineAll$q9_6) +
                                 as.numeric(baselineAll$q9_10) + (as.numeric(baselineAll$q9_11) + 
                                                                    as.numeric(baselineAll$q9_12) + as.numeric(baselineAll$q9_14) + 
                                                                    as.numeric(baselineAll$q9_15 + as.numeric(baselineAll$q9_17))*2) + 
                                 ((as.numeric(baselineAll$q9_13) + as.numeric(baselineAll$q9_19) + 
                                     baselineAll$shared_facilities)*3))))

#create column with asset quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
baselineAll$asset_score[is.na(baselineAll$asset_score)]<-0

baselineAll$asset_quintile<-as.integer(cut(baselineAll$asset_score, quantile(baselineAll$asset_score, 
                                                                             probs=0:5/5, include.lowest=TRUE)))


