#baseline descriptives



# Load data ---------------------------------------------------------------
#run B1 first, will use Q11_all dataset


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


# Household size ----------------------------------------------------------

Q11_all$child_5_17 
Q11_all$adult


# Household monthly income ------------------------------------------------


