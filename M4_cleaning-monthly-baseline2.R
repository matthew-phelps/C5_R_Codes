# Author: Matthew Phelps and Char Tamason
# Desc:   Check for problems in data that emerge once monthly and baseline
#         data are joined  
# output: Cleaned monthly visits with baseline info
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------


# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       monthly_basebase.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       monthly_basebase.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\C5_R_Codes\\Rdata\\clean-monthly-baseline.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")

# LOAD FILES --------------------------------------------------------------

load(monthly_basebase.path)
rm(monthly_basebase.path)




# INDEX PROBLEM RECORDS ---------------------------------------------------

early.visit <- m4[m4$date_visit < m4$phone.dist - 2, ]
late.visit <- m4[m4$date_visit > m4$with_date, ]



# CLEAN DATA --------------------------------------------------------------

#delete practice cases (identified by Char and confirmed by Bimal)
m4<-m4[!(m4$uniqueID=="015_2014-08-29"&m4$date_visit=="2014-11-10"),]
m4<-m4[!(m4$uniqueID=="001_2014-09-14"&m4$date_visit=="2014-11-16"),]
m4<-m4[!(m4$uniqueID=="123_2014-07-22"&m4$FRA==123),]
m4<-m4[!(m4$uniqueID=="400_2014-07-15"&m4$FRA==0),]


#methods for identifying mislabeled HHIDs in monthly visits
#View(m4[is.na(m4$ppl)|is.na(m4$FRA),]) # look at unmatched visits
#View(m4[m4$date_visit=="2015-01-14",]) #Look at date of unmatched monthly visit to see if there is a corresponding X-2 date with a different ID
#View(m4[(m4$uniqueID=="327_2014-07-12"|m4$uniqueID=="372_2014-12-05"),]) #if yes, view both IDs and compare containers
#View(m4[m4$uniqueID=="395_2014-07-17",]) #if a HHID has a missing X-2 and monthly visit, check dates


# Clean water quantities --------------------------------------------------


#look for anomalies in consumption in liters per capita per day 
for (i in 1:14) {
  m4[is.na(m4[paste("cont",i,".cont",i,"_size",sep="")]),paste("cont",i,".cont",i,"_size",sep="")] <- 0  
}

for (i in 1:14) {
  m4[is.na(m4[paste("cont",i,".cont",i,"_times",sep="")]),paste("cont",i,".cont",i,"_times",sep="")] <- 0  
}

m4$other_water_in.adult_bathe_in[is.na(m4$other_water_in.adult_bathe_in)]<-0
m4$other_water_out.adult_bathe_out[is.na(m4$other_water_out.adult_bathe_out)]<-0
m4$other_water_out.child_bathe_out[is.na(m4$other_water_out.child_bathe_out)]<-0
m4$other_water_in.child_bathe_in[is.na(m4$other_water_in.child_bathe_in)]<-0

m4$daily_volume<-with(m4, (cont1.cont1_size*cont1.cont1_times)+(cont2.cont2_size*cont2.cont2_times)+
                             (cont3.cont3_size*cont3.cont3_times)+(cont4.cont4_size*cont4.cont4_times)+(cont5.cont5_size*cont5.cont5_times)
                           +(cont6.cont6_size*cont6.cont6_times)+(cont7.cont7_size*cont7.cont7_times)+(cont8.cont8_size*cont8.cont8_times)
                           +(cont9.cont9_size*cont9.cont9_times)+(cont10.cont10_size*cont10.cont10_times)+(cont11.cont11_size*cont11.cont11_times)
                           +(cont12.cont12_size*cont12.cont12_times)+(cont13.cont13_size*cont13.cont13_times)+(cont14.cont14_size*cont14.cont14_times)
                           +((other_water_in.adult_bathe_in+other_water_out.adult_bathe_out)*37)  #will probably change once more detailed information is received from Rebeca
                           +((other_water_out.child_bathe_out+other_water_in.child_bathe_in)*14))
#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43



#create H20 per capita variable
m4$ppl[is.na(m4$ppl)]<-0
m4$ppl<-ifelse(m4$ppl==0,m4$total_HH_members,m4$ppl)
m4$daily_h2o_percapita<-with(m4, daily_volume/ppl)

#If the range of water consumption per capita is > 100 LCPD, flag for follow up
range_list <- data.frame()

for (i in 1:length(unique(m4$uniqueID))) {
  
  range_list[i,1] <- unique(m4$uniqueID)[i]
  range_list[i,2] <- range(m4[which(m4$uniqueID==unique(m4$uniqueID)[i]), "daily_h2o_percapita"])[1]
  range_list[i,3] <- range(m4[which(m4$uniqueID==unique(m4$uniqueID)[i]), "daily_h2o_percapita"])[2]
  range_list[i,4] <- range_list[i,3] - range_list[i,2]
  
}

names(range_list) <- c("uniqueID", "range_min", "range_max", "range_diff")

m4 <- merge(m4, range_list, by="uniqueID")

#write table of data where water use = 0
setwd("C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\Sent to bangladesh for clarification")
sub<-m4[which(!(is.na(m4$FRA))),]

write.csv2(sub[sub$daily_h2o_percapita==0,c("HHID", "date_visit", "FRA","ppl",
                                            "daily_volume","daily_h2o_percapita")],file="No water use.csv")

#delete the three entries with no info on water use
m4<-m4[!(m4$daily_volume==0),]

#create subset that are out of range without the ones I already checked
sub<-m4[!(m4$uniqueID=="005_2014-09-12"|m4$uniqueID=="008_2014-12-09"|m4$uniqueID=="010_2014-06-02"|m4$uniqueID=="025_2014-09-17"|
          m4$uniqueID=="083_2014-09-11"|m4$uniqueID=="094_2014-09-12"|m4$uniqueID=="108_2014-08-22"|
          m4$uniqueID=="110_2014-10-23"|m4$uniqueID=="131_2014-08-29"|m4$uniqueID=="217_2014-10-31"|
            m4$uniqueID=="218_2014-06-03"|m4$uniqueID=="249_2014-12-26"|m4$uniqueID=="293_2015-03-20"|
            m4$uniqueID=="299_2014-08-22"|m4$uniqueID=="353_2014-07-14"|m4$uniqueID=="364_2014-08-08"|
            m4$uniqueID=="369_2014-07-11"|m4$uniqueID=="388_2014-11-20"|m4$uniqueID=="396_2014-07-12"),]    
    
sub<-sub[!(is.na(sub$FRA)),]        
write.csv2(sub[(sub$range_diff>100),c("uniqueID","listing","date_visit","FRA","ppl","daily_h2o_percapita",
                   "cont1.cont1_size","cont1.cont1_times",	"cont2.cont2_size",	"cont2.cont2_times",	
                   "cont3.cont3_size",	"cont3.cont3_times","cont4.cont4_size",	"cont4.cont4_times",		
                   "cont5.cont5_size","cont5.cont5_times",	"cont6.cont6_size",	"cont6.cont6_times",
                   "cont7.cont7_size",	"cont7.cont7_times","cont8.cont8_size",	"cont8.cont8_times",	"cont9.cont9_size",
                   "cont9.cont9_times",	"cont10.cont10_size",	"cont10.cont10_times","other_water_in.adult_bathe_in",
                   "other_water_out.adult_bathe_out","other_water_in.child_bathe_in","other_water_out.child_bathe_out")],file="Range over 100.csv")

#verified visually, difference is because of a high or low wash day for uniqueIDs: 005_2014_09_12, 008_2014_12_09,
    #010_2014_06_02, 025_2014_09_17, 083_2014_09_11, 094_2014-09-12, 108_2014-08-22, 110_2014-10-23, 131_2014-08-29,
    #217_2014-10-31, 218_2014-06-03,249_2014-12-26,293_2015-03-20,299_2014-08-22, 353_2014-07-14, 364_2014-08-08,
    #369_2014-07-11, 388_2014-11-20, 396_2014-07-12, 019_2014-06-01, 001_2014-09-14, 085_2014-08-28, 281_2014-09-24

#doesn't meet criteria but was checked and verified by Bimal: 259_2014-10-20, 009_2014-06-02, 252_2014-06-01

#View(m4[m4$cont1.cont1_size==6&m4$cont2.cont2_size==2&m4$cont3.cont3_size==2,])
 #       &m4$cont2.cont2_size==22&m4$cont3.cont3_size==12&m4$cont4.cont4_size==14,])

#Erase rows that have container information that doesn't match up anywhere
m4<-m4[!(m4$uniqueID=="001_2014-09-14"&m4$date_visit==16-11-2014),]
m4<-m4[!(m4$uniqueID=="028_2014-09-11"&m4$date_visit==18-11-2014),]



m4$cont3.cont3_times[m4$cont3.cont3_times==20&m4$uniqueID=="138_2014-07-21"]<-2 #keystroke error, should be 2, not 20
m4$cont3.cont3_times[m4$cont3.cont3_times==8&m4$uniqueID=="344_2014-11-10"]<-18 #keystroke error, all other container 3 are 18 liters
m4$cont7.cont7_size[m4$cont7.cont7_size==212&m4$uniqueID=="219_2014-08-04"]<-21 #keystroke error, only entry missing a 21 liter container

m4$ppl[m4$ppl==5&m4$uniqueID=="156_2014-08-06"]<-1 #all water use data is same but people is 5 instead of 1

#Check data on activities done without a container
m4$bath_pc<-with(m4,(other_water_in.adult_bathe_in+other_water_out.adult_bathe_out+other_water_in.child_bathe_in+other_water_out.child_bathe_out)/m4$ppl)

#check that all houses with child baths have children
# m4$bath_child<-with(m4,(other_water_in.child_bathe_in+other_water_out.child_bathe_out))
# write.csv2(m4[m4$bath_child>=1&m4$children==0,c("new_per","new_per1_age","HHID", 
#                                                                "date_visit", "FRA","ppl","other_water_in.adult_bathe_in","other_water_out.adult_bathe_out",
#                                                                "other_water_in.child_bathe_in","other_water_out.child_bathe_out")],
#            file="Too many child baths.csv")
# 
# 
# m4[m4$HHID==246&m4$new_per==2,c("new_per2_age")]

#setwd("C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\Sent to bangladesh for clarification")
# #write.csv2(m4[m4$bath_pc>3,c("HHID", "date_visit", "FRA","ppl","other_water_in.adult_bathe_in","other_water_out.adult_bathe_out","other_water_in.child_bathe_in","other_water_out.child_bathe_out")], 
#            file="Too many baths.csv")
# 

m4$other_water_in.wash_plate_in[is.na(m4$other_water_in.wash_plate_in)]<-0
m4$other_water_out.wash_plate_out[is.na(m4$other_water_out.wash_plate_out)]<-0

# m4$dishes<-with(m4,(other_water_in.wash_plate_in+other_water_out.wash_plate_out))
# write.csv2(m4[m4$dishes>5,c("HHID", "date_visit", "FRA","ppl","other_water_in.wash_plate_in",
#                                       "other_water_out.wash_plate_out")],file="Washed dishes.csv")

# m4[m4$dishes>0,c("HHID")]
# 
m4$other_water_in.wash_clothes_in[is.na(m4$other_water_in.wash_clothes_in)]<-0
m4$other_water_out.wash_clothes_out[is.na(m4$other_water_out.wash_clothes_out)]<-0

# m4$wash_clothes_total<-with(m4,(other_water_in.wash_clothes_in+other_water_out.wash_clothes_out))
# write.csv2(m4[m4$clothes>5,c("HHID", "date_visit", "FRA","ppl","other_water_in.wash_clothes_in",
#                                        "other_water_out.wash_clothes_out","wash_clothes_total")],file="Too many clothes.csv")

# range(m4[m4$clothes>0,c("clothes")])

m4[which(m4$other_water_in.child_bathe_in==6&m4$uniqueID=="269_2015-02-17"),"other_water_in.child_bathe_in"]<-0

# cleaning water source information ---------------------------------------
#for primary water source from monthly visit data

#View(m4[m4$water_point1.wa_pt1==777&m4$water_point1.wa_pt1_usebefore==2,c("water_point1.wa_pt1","water_point1.wa_source1","water_point1.wa_source1_other","water_point1.wa_tank1")])

m4$h2o_collect1<-with(m4, ifelse(water_point1.wa_pt1==1|water_point1.wa_pt1==2,1, #tap/pipe
                                           ifelse(water_point1.wa_pt1==3|water_point1.wa_pt1==4,2, #handpump
                                                  ifelse(water_point1.wa_pt1==5|water_point1.wa_pt1==777,3,4)))) #well, all 777 were checked, and reported bucket in tank category 
m4$h2o_collect1[is.na(m4$h2o_collect1)]<-0 #set NAs to 0 for the loop

m4$h2o_source1<-with(m4, ifelse(water_point1.wa_source1==1|water_point1.wa_source1==999,1, #WASA, only 999 was a handpump
                                          ifelse(water_point1.wa_source1==2|water_point1.wa_source1==3|water_point1.wa_source1==6,2, #Deep tube well
                                                 ifelse(water_point1.wa_source1==4|water_point1.wa_source1==5,3,0))))
m4$h2o_source1[is.na(m4$h2o_source1)]<-0 #set NAs to 0 for the loop

m4$h2o_tank1<-ifelse(m4$water_point1.wa_tank1>0,1,2)
m4$h2o_tank1[is.na(m4$h2o_tank1)]<-0 #set NAs to 0 for the loop

m4$water_point1.wa_pt1_usebefore<-ifelse(m4$first_visit==1,3,m4$water_point1.wa_pt1_usebefore)



m4$base_source_change <- ifelse(m4$first_visit==1 & m4$q14_recoded==m4$h2o_collect1 &
                                       m4$q14a_recoded == m4$h2o_source1 & m4$q15_recoded==m4$h2o_tank1, "No", ifelse(m4$first_visit!=1, NA, "Yes"))

m4<-m4[which(!(is.na(m4$FRA))),] #get rid of all x-2 entries that don't have corresponding monthly visit


m4_subset<-data.frame()


# loop to fill in values for primary water source info in subsequent visits 
for (i in 1:length(unique(m4$uniqueID))) {
  
  m4_subset <- m4[which(m4$uniqueID==unique(m4$uniqueID)[i]),]
  m4_subset <- m4_subset[order((m4_subset$date_visit)),]
  
  clean <- function(x) {
    
    for (j in 1:(nrow(x)-1)) {
      x[j+1, "h2o_collect1"] <- ifelse(x[j+1, "water_point1.wa_pt1_usebefore"]==1,
                                       x[j, "h2o_collect1"],
                                       x[j+1, "h2o_collect1"])
      x[j+1, "h2o_source1"] <- ifelse(x[j+1, "water_point1.wa_pt1_usebefore"]==1,
                                      x[j, "h2o_source1"],
                                      x[j+1, "h2o_source1"])  
      x[j+1, "h2o_tank1"] <- ifelse(x[j+1, "water_point1.wa_pt1_usebefore"]==1,
                                    x[j, "h2o_tank1"],
                                    x[j+1, "h2o_tank1"]) 
    }
    
    return (as.data.frame(x))
  }
  
  # condition 1: 3 & 1s (first visit with no following changes) or 3 & 2 (first visit followed by change)>>> control stcutures in R
  
  if (length(unique(m4_subset$water_point1.wa_pt1_usebefore))==2 &
      unique((m4_subset$water_point1.wa_pt1_usebefore))[1]==3 & 
      unique((m4_subset$water_point1.wa_pt1_usebefore))[2]==1){m4_subset <- clean(m4_subset[i])}
  
  if(m4_subset$water_point1.wa_pt1_usebefore==1&m4_subset$h2o_source1[h]==m4_subset$h2o_source1[h-1]){}
  
  else  {m4_subset}}
    # condition 2
    {if(length(unique(m4_subset$water_point1.wa_pt1_usebefore))==2 &
                  unique(m4_subset$water_point1.wa_pt1_usebefore)[1]==3 & 
                  unique(m4_subset$water_point1.wa_pt1_usebefore)[2]==2) {m4_subset <- clean(m4_subset[i])  }
  
  else {(m4_subset)}
  }}

m4_subset$h2o_tank1

else  if    { (length(unique(m4_subset$water_point1.wa_pt1_usebefore))==3 &
                     unique(m4_subset$water_point1.wa_pt1_usebefore)[1]==3 & 
                     unique(m4_subset$water_point1.wa_pt1_usebefore)[2]==1
                     unique(m4_subset$water_point1.wa_pt1_usebefore)[3]==2) # condition 2
    { }

else  if    { (length(unique(m4_subset$water_point1.wa_pt1_usebefore))==3 &
                 unique(m4_subset$water_point1.wa_pt1_usebefore)[1]==3 & 
                 unique(m4_subset$water_point1.wa_pt1_usebefore)[2]==2
                 unique(m4_subset$water_point1.wa_pt1_usebefore)[3]==1) # condition 2
{ }

}}


# Clean hours per day that water is flowing from source -------------------

monthly$water_flow1_end1<- times(monthly$water_point1.wa_flow1.wa_time1.aE)
monthly$water_flow1_start1<-times(monthly$water_point1.wa_flow1.wa_time1.aS)
monthly$water_flow2_end2<- times(monthly$water_point1.wa_flow1.wa_time1.bE)
monthly$water_flow2_start2<-times(monthly$water_point1.wa_flow1.wa_time1.bS)
monthly$water_flow3_end3<- times(monthly$water_point1.wa_flow1.wa_time1.cE)
monthly$water_flow3_start3<-times(monthly$water_point1.wa_flow1.wa_time1.cS)


monthly$water_flow_1<- monthly$water_flow1_end1-monthly$water_flow1_start1
monthly$water_flow_1<-monthly$water_flow_1*24
monthly$water_flow_1<-ifelse((monthly$water_flow1_end1==times("17:59:00")&monthly$water_flow1_start1==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings and daylight savings time
                             |(monthly$water_flow1_end1==times("15:59:00")&monthly$water_flow1_start1==times("16:00:00")),24,
                             monthly$water_flow_1)

monthly$water_flow_1<-ifelse(monthly$water_flow_1<0,monthly$water_flow_1+24,monthly$water_flow_1)


monthly$water_flow_2<- monthly$water_flow2_end2-monthly$water_flow2_start2
monthly$water_flow_2<-monthly$water_flow_2*24
monthly$water_flow_2<-ifelse((monthly$water_flow2_end2==times("17:59:00")&monthly$water_flow2_start2==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings
                             |(monthly$water_flow2_end2==times("15:59:00")&monthly$water_flow2_start2==times("16:00:00")),24,
                             monthly$water_flow_2)
monthly$water_flow_2<-ifelse(monthly$water_flow_2<0,monthly$water_flow_2+24,monthly$water_flow_2)

monthly$water_flow_3<- monthly$water_flow3_end3-monthly$water_flow3_start3
monthly$water_flow_3<-monthly$water_flow_3*24
monthly$water_flow_3<-ifelse((monthly$water_flow3_end3==times("17:59:00")&monthly$water_flow3_start3==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings 
                             |(monthly$water_flow3_end3==times("15:59:00")&monthly$water_flow3_start3==times("16:00:00")),24,
                             monthly$water_flow_3)
monthly$water_flow_3<-ifelse(monthly$water_flow_3<0,monthly$water_flow_3+24,monthly$water_flow_3)

monthly$round<-round(monthly$water_flow_1,3) #need to round decimal points or replacement doesn't work
monthly$round2<-round(monthly$water_flow_2,3)
monthly$round3<-round(monthly$water_flow_3,3)



#correct obvious typos, data collectors were instructed to round to 15 minute intervals when applicable
monthly$water_flow_1[which(monthly$round==23.600)]<-0
monthly$water_flow_1[which(monthly$round==23.933)]<-24
monthly$water_flow_1[which(monthly$round==23.483)]<-24
monthly$water_flow_1[which(monthly$round==23.150)]<-24
monthly$water_flow_1[which(monthly$round==23.850)]<-24
monthly$water_flow_1[which(monthly$round==23.967)]<-24
monthly$water_flow_1[which(monthly$round==23.483)]<-24
monthly$water_flow_1[which(monthly$round==15.983)]<-24
monthly$water_flow_1[which(monthly$round==13.983)]<-24

monthly$water_flow_2[which(monthly$round>23.9)]<-0
monthly$water_flow_3[which(monthly$round>23.9)]<-0

# View(monthly[monthly$water_flow_1>23&monthly$water_flow_1<24,c("water_flow_1","round",
#                       "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE",
#                       "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE")])

monthly$checkwater<-monthly$water_flow_1+monthly$water_flow_2+monthly$water_flow_3

# write.csv2(monthly[monthly$checkwater>24,c("uniqueID","Listing.number.x","date_visit",
#"water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
#"water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
#"water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="too many hours of water use.csv")

write.csv2(monthly[monthly$round2==0.017|monthly$round==0.667|monthly$round==16.517,c("uniqueID","FRA","Listing.number.x","date_visit",
                                                                                      "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
                                                                                      "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
                                                                                      "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="water flow recheck.csv")


# RE-CHECK CLEAN DATA -----------------------------------------------------



# SAVE --------------------------------------------------------------------

save(m4, file = data.output.path)

