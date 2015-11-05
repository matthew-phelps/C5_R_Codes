# Author: Matthew Phelps and Char Tamason
# Desc:   Check for problems in data that emerge once monthly and baseline
#         data are joined  
# output: Cleaned monthly visits with baseline info
# DEPENDENCIES: Requires M1, M2, M3 & B1, B2 to have been run


# Intro -------------------------------------------------------------------
library(chron)

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())
ifelse(grepl("zrc340", getwd()),
       monthly_basebase.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata",
       monthly_basebase.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\dirty-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()),
       qualitative.path <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data\\C5 Monthly Visits data\\Water use observations from Rebeca",
       qualitative.path <-"C:\\Users\\wrz741\\Dropbox\\C5 data\\C5 Monthly Visits data\\Water use observations from Rebeca")
ifelse(grepl("zrc340", getwd()),
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata",
       data.output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")

# LOAD FILES --------------------------------------------------------------

load(monthly_basebase.path)
rm(monthly_basebase.path)

setwd(qualitative.path)
Qual<-read.csv2("Rebeca's qualitative data to import.csv")

# INDEX PROBLEM RECORDS ---------------------------------------------------

early.visit <- m4[m4$date_visit < m4$phone.dist - 2, ]
late.visit <- m4[m4$date_visit > m4$with_date, ]



# CLEAN DATA --------------------------------------------------------------

#delete practice cases (identified by Char and confirmed by Bimal)
m4<-m4[!(m4$uniqueID=="015_2014-08-29"&m4$date_visit=="2014-11-10"),]
m4<-m4[!(m4$uniqueID=="001_2014-09-14"&m4$date_visit=="2014-11-16"),]
m4<-m4[!(m4$uniqueID=="062_2014-10-14"&m4$date_visit=="04-02-2015"),]
m4<-m4[!(m4$uniqueID=="123_2014-07-22"&m4$FRA==123),]
m4<-m4[!(m4$uniqueID=="400_2014-07-15"&m4$FRA==0),]


#methods for identifying mislabeled HHIDs in monthly visits
#View(m4[is.na(m4$ppl)|is.na(m4$FRA),]) # look at unmatched visits
#View(m4[m4$date_visit=="2014-11-18",]) #Look at date of unmatched monthly visit to see if there is a corresponding X-2 date with a different ID
#View(m4[(m4$uniqueID=="327_2014-07-12"|m4$uniqueID=="372_2014-12-05"),]) #if yes, view both IDs and compare containers
#View(m4[m4$uniqueID=="395_2014-07-17",]) #if a HHID has a missing X-2 and monthly visit, check dates
#View(m4[m4$cont4.cont4_size==15&m4$cont1.cont1_size==2.5,])
m4<-m4[!(is.na(m4$uniqueID)),]

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


m4$dishes<-m4$other_water_in.wash_plate_in+m4$other_water_out.wash_plate_out
m4[is.na(m4$dishes),c("dishes")]<-0
m4$clothes<-m4$other_water_in.wash_clothes_in+m4$other_water_out.wash_clothes_out
m4[is.na(m4$clothes),c("clothes")]<-0
#average water consumption per activity in liters: adult bath= 37, child bath = 14, wash dishes = 25, wash clothes =43

#create month variable
#month variable
m4$date_visit_character<-as.character(m4$date_visit)
temp<-strsplit(m4$date_visit_character, "-")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("year", "month", "day")
m4<- cbind(m4,df)

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
#setwd("C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\Sent to bangladesh for clarification")
#sub<-m4[which(!(is.na(m4$FRA))),]

#create subset that are out of range without the ones I already checked
# sub<-m4[!(m4$uniqueID=="005_2014-09-12"|m4$uniqueID=="008_2014-12-09"|m4$uniqueID=="010_2014-06-02"|m4$uniqueID=="025_2014-09-17"|
#           m4$uniqueID=="083_2014-09-11"|m4$uniqueID=="094_2014-09-12"|m4$uniqueID=="108_2014-08-22"|
#           m4$uniqueID=="110_2014-10-23"|m4$uniqueID=="131_2014-08-29"|m4$uniqueID=="217_2014-10-31"|
#             m4$uniqueID=="218_2014-06-03"|m4$uniqueID=="249_2014-12-26"|m4$uniqueID=="293_2015-03-20"|
#             m4$uniqueID=="299_2014-08-22"|m4$uniqueID=="353_2014-07-14"|m4$uniqueID=="364_2014-08-08"|
#             m4$uniqueID=="369_2014-07-11"|m4$uniqueID=="388_2014-11-20"|m4$uniqueID=="396_2014-07-12"),]    
#     
# sub<-sub[!(is.na(sub$FRA)),]        
#write.csv2(sub[(sub$range_diff>100),c("uniqueID","listing","date_visit","FRA","ppl","daily_h2o_percapita",
#                    "cont1.cont1_size","cont1.cont1_times",	"cont2.cont2_size",	"cont2.cont2_times",	
#                    "cont3.cont3_size",	"cont3.cont3_times","cont4.cont4_size",	"cont4.cont4_times",		
#                    "cont5.cont5_size","cont5.cont5_times",	"cont6.cont6_size",	"cont6.cont6_times",
#                    "cont7.cont7_size",	"cont7.cont7_times","cont8.cont8_size",	"cont8.cont8_times",	"cont9.cont9_size",
#                    "cont9.cont9_times",	"cont10.cont10_size",	"cont10.cont10_times","other_water_in.adult_bathe_in",
#                    "other_water_out.adult_bathe_out","other_water_in.child_bathe_in","other_water_out.child_bathe_out")],file="Range over 100.csv")

#verified visually, difference is because of a high or low wash day for uniqueIDs: 005_2014_09_12, 008_2014_12_09,
    #010_2014_06_02, 025_2014_09_17, 083_2014_09_11, 094_2014-09-12, 108_2014-08-22, 110_2014-10-23, 131_2014-08-29,
    #217_2014-10-31, 218_2014-06-03,249_2014-12-26,293_2015-03-20,299_2014-08-22, 353_2014-07-14, 364_2014-08-08,
    #369_2014-07-11, 388_2014-11-20, 396_2014-07-12, 019_2014-06-01, 001_2014-09-14, 085_2014-08-28, 281_2014-09-24

#doesn't meet criteria but was checked and verified by Bimal: 259_2014-10-20, 009_2014-06-02, 252_2014-06-01

#View(m4[m4$cont1.cont1_size==6&m4$cont2.cont2_size==2&m4$cont3.cont3_size==2,])
 #       &m4$cont2.cont2_size==22&m4$cont3.cont3_size==12&m4$cont4.cont4_size==14,])


m4$cont3.cont3_times[m4$cont3.cont3_times==20&m4$uniqueID=="138_2014-07-21"]<-2 #keystroke error, should be 2, not 20
m4$cont3.cont3_times[m4$cont3.cont3_times==8&m4$uniqueID=="344_2014-11-10"]<-18 #keystroke error, all other container 3 are 18 liters
m4$cont7.cont7_size[m4$cont7.cont7_size==212&m4$uniqueID=="219_2014-08-04"]<-21 #keystroke error, only entry missing a 21 liter container

m4$ppl[m4$ppl==5&m4$uniqueID=="156_2014-08-06"]<-1 #all water use data is same but people is 5 instead of 1

#correct errors checked by Bimal in water use

m4$cont1.cont1_size[m4$uniqueID=="052_2014-09-05"&m4$date_visit=="2014-12-01"]<-23
m4$date_visit[m4$uniqueID=="052_2014-09-05"&m4$date_visit=="2014-12-01"]<-as.Date("2014-11-18")
m4$cont2.cont2_size[m4$uniqueID=="067_2014-06-05"&m4$date_visit=="2014-09-24"]<-24
m4$cont1.cont1_times[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-4
m4$cont2.cont2_size[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-2
m4$cont2.cont2_times[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-3
m4$cont3.cont3_times[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-2
m4$cont3.cont3_times[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-2
m4$other_water_in.adult_bathe_in[m4$uniqueID=="156_2014-08-06"&m4$date_visit=="2014-09-16"]<-0
m4$cont2.cont2_times[m4$uniqueID=="257_2014-09-28"&m4$date_visit=="2014-12-23"]<-4
m4$other_water_out.adult_bathe_out[m4$uniqueID=="360_2014-07-16"&m4$date_visit=="2014-11-14"]<-0
#correction from Bimal from sheet "too many clothes"
m4[m4$HHID==143&m4$date_visit=="2015-06-02",c("clothes")]<-2
m4[m4$HHID==327&m4$date_visit=="2015-04-07",c("clothes")]<-3
m4[m4$HHID==400&m4$date_visit=="2015-05-08",c("clothes")]<-0


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

#water use = 0, only three entries once printed
#write.csv2(sub[sub$daily_h2o_percapita==0,c("HHID", "date_visit", "FRA","ppl",
#  "daily_volume","daily_h2o_percapita")],file="No water use.csv")

# delete the three entries where water use = 0
m4<-m4[!(m4$daily_h2o_percapita==0),]

m4[which(m4$other_water_in.child_bathe_in==6&m4$uniqueID=="269_2015-02-17"),"other_water_in.child_bathe_in"]<-0

# cleaning water source information ---------------------------------------
#for primary water source from monthly visit data

#View(m4[m4$water_point1.wa_pt1==777&m4$water_point1.wa_pt1_usebefore==2,c("water_point1.wa_pt1","water_point1.wa_source1","water_point1.wa_source1_other","water_point1.wa_tank1")])

m4$h2o_collect1<-with(m4, ifelse(water_point1.wa_pt1==1|water_point1.wa_pt1==2,1, #tap/pipe
                                 ifelse(water_point1.wa_pt1==3|water_point1.wa_pt1==4,2, #handpump
                                        ifelse(water_point1.wa_pt1==5|water_point1.wa_pt1==777,3,4)))) #well, all 777 were checked, and reported bucket in tank category 
m4$h2o_collect1[is.na(m4$h2o_collect1)]<-0 #set NAs to 0 for the loop

m4$h2o_collect2<-with(m4, ifelse(water_point2.wa_pt2==1|water_point2.wa_pt2==2,1, #tap/pipe
                                 ifelse(water_point2.wa_pt2==3|water_point2.wa_pt2==4,2, #handpump
                                        ifelse(water_point2.wa_pt2==5|water_point2.wa_pt2==777,3,4)))) #well, all 777 were checked, and reported bucket in tank category 
m4$h2o_collect2[is.na(m4$h2o_collect2)]<-0 #set NAs to 0 for the loop

m4$h2o_source1<-with(m4, ifelse(water_point1.wa_source1==1|water_point1.wa_source1==999,1, #WASA, only 999 was a handpump
                                ifelse(water_point1.wa_source1==2|water_point1.wa_source1==3|water_point1.wa_source1==6,2, #Deep tube well
                                       ifelse(water_point1.wa_source1==4|water_point1.wa_source1==5,3,0))))
m4$h2o_source1[is.na(m4$h2o_source1)]<-0 #set NAs to 0 for the loop

m4$h2o_source2<-with(m4, ifelse(water_point2.wa_source2==1|water_point2.wa_source2==999,1, #WASA, only 999 was a handpump
                                ifelse(water_point2.wa_source2==2|water_point2.wa_source2==3|water_point2.wa_source2==6,2, #Deep tube well
                                       ifelse(water_point2.wa_source2==4|water_point2.wa_source2==5,3,0))))
m4$h2o_source2[is.na(m4$h2o_source2)]<-0 #set NAs to 0 for the loop

m4$h2o_tank1<-ifelse(m4$water_point1.wa_tank1>0,1,2)
m4$h2o_tank1[is.na(m4$h2o_tank1)]<-0 #set NAs to 0 for the loop
m4$q15_recoded<-ifelse(m4$q15_recoded>0,1,2)

m4$h2o_tank2<-ifelse(m4$water_point2.wa_tank2>0,1,2)
m4$h2o_tank2[is.na(m4$h2o_tank2)]<-0 #set NAs to 0 for the loop

m4$water_point1.wa_pt1_usebefore<-ifelse(m4$first_visit==1,3,m4$water_point1.wa_pt1_usebefore)

m4$base_source_change <- ifelse(m4$first_visit==1 & m4$q14_recoded==m4$h2o_collect1 &
                                       m4$q14a_recoded == m4$h2o_source1 & m4$q15_recoded==m4$h2o_tank1, "No", ifelse(m4$first_visit!=1, NA, "Yes"))

m4$h2o_distance1<-m4$distance_to_source1
m4<-m4[which(!(is.na(m4$FRA))),] #get rid of all x-2 entries that don't have corresponding monthly visit


m4_subset<-data.frame()

#make it so that the first entry for each uniqueID starts with 3 (for loop to follow)
clean_first_entry <- function(x) {
  
  x[1, c("water_point1.wa_pt1_usebefore_new")] <- ifelse (x[1, c("water_point1.wa_pt1_usebefore")]!='3','3', x[1, c("water_point1.wa_pt1_usebefore")])
  x[1, c("h2o_source1")] <- ifelse(x[1,c("h2o_source1")]==0,x[1,c("q14a_recoded")],x[1,c("h2o_source1")])
  x[1, c("h2o_tank1")] <- ifelse(x[1,c("h2o_tank1")]==0,x[1,c("q15_recoded")],x[1,c("h2o_tank1")])
  x[1, c("h2o_distance1")] <- ifelse(x[1,c("h2o_collect1")]!=x[1,c("q14_recoded")],x[1,c("distance_to_source2")],x[1,c("distance_to_source1")])
  x[2:nrow(x), "water_point1.wa_pt1_usebefore_new"] <- ifelse(x[2:nrow(x), "water_point1.wa_pt1_usebefore"]==3,1,x[2:nrow(x), c("water_point1.wa_pt1_usebefore")])
  return(x)
}


# Fill in values for water source and tank for each visit  -------------------
# in routine/monthly visits, water source that does not change does not have info filled out


clean_all <- function(x) {
  
  for (j in 1:(nrow(x)-1)) {
    x[j+1, c("h2o_source1")] <- ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                         x[j+1, c("h2o_collect1")]==x[1, c("h2o_collect1")],
                                       x[1, c("h2o_source1")],
                                       
                                       ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                                x[j+1, c("h2o_collect1")]==x[1, c("h2o_collect2")],
                                              x[1, c("h2o_source2")],
                                       
                                       ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                                x[j+1, c("h2o_collect1")]==x[1, c("q14_recoded")],
                                              x[1, c("q14a_recoded")],
                                              
                                              ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                                       x[j+1, c("h2o_collect1")]==x[1, c("q14_recoded2")],
                                                     x[1, c("q14a_recoded2")],x[j+1, c("h2o_source1")]))))
    x[j+1, c("h2o_tank1")] <- ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                       x[j+1, c("h2o_collect1")]==x[1, c("h2o_collect1")],
                                     x[1, c("h2o_tank1")],
                                     
                                     ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                              x[j+1, c("h2o_collect1")]==x[1, c("h2o_collect2")],
                                            x[1, c("h2o_tank2")],
                                     
                                     ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                              x[j+1, c("h2o_collect1")]==x[1, c("q14_recoded")],
                                            x[1, c("q15_recoded")],
                                            
                                            ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                                     x[j+1, c("h2o_collect1")]==x[1, c("q14_recoded2")],
                                                   x[1, c("q15_recoded2")],x[j+1, c("h2o_tank1")]))))
    
    x[j+1, c("h2o_distance1")] <- ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                       x[j+1, c("h2o_collect1")]==x[1, c("h2o_collect1")],
                                     x[1, c("h2o_distance1")],
                                     
                                     ifelse(x[j+1, c("water_point1.wa_pt1_usebefore_new")]==1&
                                              x[j+1, c("h2o_collect1")]!=x[1, c("h2o_collect1")],
                                            x[1, c("distance_to_source2")],x[1, c("distance_to_source1")]))
    
  }
  
  return (as.data.frame(x))
}
  


# ------------------------- 
#m4[m4$uniqueID=="002_2014-06-11",c("uniqueID", "water_point1.wa_pt1_usebefore","water_point1.wa_pt1_usebefore_new", "h2o_collect1", "h2o_collect2", "h2o_source1", "h2o_tank1")]

m4_subset_final<-data.frame()
i<-NULL
#m4_subset_final[1:100,c("uniqueID", "water_point1.wa_pt1_usebefore_new", "h2o_collect1", "h2o_collect2", "h2o_source1", "h2o_tank1")]
# loop to fill in values for primary water source info in subsequent visits 

for (i in 1:length(unique(m4$uniqueID))) {
  
  m4_subset <- m4[which(m4$uniqueID==unique(m4$uniqueID)[i]),]
  m4_subset <- m4_subset[order((m4_subset$date_visit)),]
  m4_subset <- clean_first_entry(m4_subset)  
  
  if (unique(m4_subset$water_point1.wa_pt1_usebefore_new)[1]==3) {
    
    m4_subset <- clean_all(m4_subset)
    
  }
  
  m4_subset_final <- rbind(m4_subset_final, m4_subset)
  
}

m4<-m4_subset_final[!(is.na(m4_subset_final$uniqueID)),]

#View(m4[,c("uniqueID", "water_point1.wa_pt1_usebefore_new", "h2o_collect1", "h2o_source1", "h2o_tank1","h2o_distance1")])

#check NAs
#m4[is.na(m4$h2o_distance1)&!(is.na(m4$h2o_source1)),c("uniqueID","h2o_source1","distance_to_source1")]

#correct NA values for distance when only 1 source reported
m4[m4$uniqueID=="008_2014-12-09",c("h2o_distance1")]<-10.50
m4[m4$uniqueID=="074_2014-09-05",c("h2o_distance1")]<-1.20
m4[m4$uniqueID=="103_2014-06-09",c("h2o_distance1")]<-8.40
m4[m4$uniqueID=="161_2014-11-13",c("h2o_distance1")]<-6.30
m4[m4$uniqueID=="175_2014-08-22",c("h2o_distance1")]<-4.30
m4[m4$uniqueID=="247_2014-08-22",c("h2o_distance1")]<-14.30
m4[m4$uniqueID=="325_2014-08-14",c("h2o_distance1")]<-2.10


# Clean hours per day that water is flowing from source -------------------

m4$water_flow1_end1<- times(m4$water_point1.wa_flow1.wa_time1.aE)
m4$water_flow1_start1<-times(m4$water_point1.wa_flow1.wa_time1.aS)
m4$water_flow2_end2<- times(m4$water_point1.wa_flow1.wa_time1.bE)
m4$water_flow2_start2<-times(m4$water_point1.wa_flow1.wa_time1.bS)
m4$water_flow3_end3<- times(m4$water_point1.wa_flow1.wa_time1.cE)
m4$water_flow3_start3<-times(m4$water_point1.wa_flow1.wa_time1.cS)


m4$water_flow_1<- m4$water_flow1_end1-m4$water_flow1_start1
m4$water_flow_1<-m4$water_flow_1*24
m4$water_flow_1<-ifelse((m4$water_flow1_end1==times("17:59:00")&m4$water_flow1_start1==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings and daylight savings time
                             |(m4$water_flow1_end1==times("15:59:00")&m4$water_flow1_start1==times("16:00:00")),24,
                             m4$water_flow_1)

m4$water_flow_1<-ifelse(m4$water_flow_1<0,m4$water_flow_1+24,m4$water_flow_1)


m4$water_flow_2<- m4$water_flow2_end2-m4$water_flow2_start2
m4$water_flow_2<-m4$water_flow_2*24
m4$water_flow_2<-ifelse((m4$water_flow2_end2==times("17:59:00")&m4$water_flow2_start2==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings
                             |(m4$water_flow2_end2==times("15:59:00")&m4$water_flow2_start2==times("16:00:00")),24,
                             m4$water_flow_2)
m4$water_flow_2<-ifelse(m4$water_flow_2<0,m4$water_flow_2+24,m4$water_flow_2)

m4$water_flow_3<- m4$water_flow3_end3-m4$water_flow3_start3
m4$water_flow_3<-m4$water_flow_3*24
m4$water_flow_3<-ifelse((m4$water_flow3_end3==times("17:59:00")&m4$water_flow3_start3==times("18:00:00")) # time ending in 1 minute less than start time indicates 24 hours. is different because of tablet settings 
                             |(m4$water_flow3_end3==times("15:59:00")&m4$water_flow3_start3==times("16:00:00")),24,
                             m4$water_flow_3)
m4$water_flow_3<-ifelse(m4$water_flow_3<0,m4$water_flow_3+24,m4$water_flow_3)

m4$round<-round(m4$water_flow_1,3) #need to round decimal points or replacement doesn't work
m4$round2<-round(m4$water_flow_2,3)
m4$round3<-round(m4$water_flow_3,3)



#correct obvious typos, data collectors were instructed to round to 15 minute intervals when applicable
m4$water_flow_1[which(m4$round==23.600)]<-0
m4$water_flow_1[which(m4$round==23.933)]<-24
m4$water_flow_1[which(m4$round==23.483)]<-24
m4$water_flow_1[which(m4$round==23.150)]<-24
m4$water_flow_1[which(m4$round==23.850)]<-24
m4$water_flow_1[which(m4$round==23.967)]<-24
m4$water_flow_1[which(m4$round==23.483)]<-24
m4$water_flow_1[which(m4$round==15.983)]<-24
m4$water_flow_1[which(m4$round==13.983)]<-24

m4$water_flow_2[which(m4$round>23.9)]<-0
m4$water_flow_3[which(m4$round>23.9)]<-0

# View(m4[m4$water_flow_1>23&m4$water_flow_1<24,c("water_flow_1","round",
#                       "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE",
#                       "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE")])

m4$checkwater<-m4$water_flow_1+m4$water_flow_2+m4$water_flow_3

#write file to send to bangladesh where water use is more than 24 hours in a day
# write.csv2(m4[m4$checkwater>24,c("uniqueID","Listing.number.x","date_visit","FRA"
# "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
# "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
# "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="too many hours of water use.csv")

#write file to send to bangladesh from FRA 9446 that appears to have messed up several entries
# write.csv2(m4[m4$FRA==9446,c("uniqueID","Listing.number.x","date_visit","FRA",
# "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
# "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
# "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="FRA 9446.csv")
# 
#write file to send to bangladesh where water use is 0
# write.csv2(m4[m4$daily_volume==0,c("uniqueID","Listing.number.x","date_visit","FRA",
#                                    "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
#                                    "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
#                                    "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="No water use.csv")
# 

#write.csv2(m4[m4$round2==0.017|m4$round==0.667|m4$round==16.517,c("uniqueID","FRA","Listing.number.x","date_visit",
#                                                                                       "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
#                                                                                       "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
#                                                                                       "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3","checkwater")], file="water flow recheck.csv")


# Calculate quantities based on Rebeca's data -----------------------------
#m4$distance_to_source1
#m4[m4$Listing.number.x==968,c("checkwater")]
Qual$Listing.number.x<-Qual$Listing.ID
merged<-merge(Qual,m4,by="Listing.number.x") # one overlapping listing number, rebeca confirmed that hhid 241 was included, not 013 
merged<-merged[!(merged$HHID==013),]
merged<-merged[!duplicated(merged$uniqueID),]

merged[is.na(merged$number_of_child_bath),c("number_of_child_bath")]<-0
merged$number_adults <- merged$ppl.x-merged$number_of_child_bath
merged[merged$Listing.number.x==1190,c("number_adults")]<-2

merged$water_use_pc<-merged$total_water_use/merged$ppl.x
merged$adults_baths<-merged$water_quant_per_adult_bath
merged$water_quant_per_adult_bath<-merged$adults_baths/merged$number_adults

merged$access_groups<- with(merged,ifelse(Listing.number.x==1089|Listing.number.x==1190|Listing.number.x==958,"a",
                                           ifelse(Listing.number.x==1422|Listing.number.x==947|Listing.number.x==155|Listing.number.x==403,"b",
                                                  ifelse(Listing.number.x==1027|Listing.number.x==975,"c","d"))))

merged$all_day_water<- with(merged,ifelse(access_groups=="a"|access_groups=="b","no","yes"))

test<-aov(water_use_pc~access_groups, data=merged)
summary(test) # p=0.68, no sig difference
test2<-aov(water_use_pc~all_day_water, data=merged)
summary(test2) # p=0.26, no sig difference

#calculate water used for only rinsing
merged$clothes_wash_soap_times <- merged$number_of_times_clotheswash-merged$number_adults
merged$clothes_rinse_times <- merged$number_of_times_clotheswash-merged$clothes_wash_soap_times
merged$clothes_water_per_wash_sub <- merged$water_quant_daily_clothes_wash/merged$number_of_times_clotheswash

merged$only_rinses<- ifelse(merged$clothes_wash_soap_times<=0,"rinse_only","rinse_and_wash")
sub<-subset(merged[merged$clothes_wash_soap_times<=0,]) 
sub2<-subset(merged[merged$clothes_wash_soap_times>0,]) 
merged[,c("access_groups","only_rinses","clothes_water_per_wash_sub")]
mean(sub$clothes_water_per_wash_sub)
mean(sub2$clothes_water_per_wash_sub)


# merged[merged$q14_recoded==1,c("water_quant_per_adult_bath","distance_to_source1")]
# 
# merged[,c("uniqueID","distance_to_source1","Listing.number.x","q14_recoded","q15_recoded","h2o_collect1","checkwater")]


#group 1 less than 24 hours tap
#group 2 <24 hour service with handpump

#Cleaning from Bimal's correction on households with no recorded water use
m4[m4$HHID==018&m4$date_visit==as.Date("2015-05-29"),c("cont1.cont1_size")]<-118.5 #actually sum of containers but too tedious to input all
m4[m4$HHID==018&m4$date_visit==as.Date("2015-05-29"),c("cont1.cont1_times")]<-1
m4[m4$HHID==018&m4$date_visit==as.Date("2015-05-29"),c("other_water_in.wash_clothes_in")]<-3
m4[m4$HHID==018&m4$date_visit==as.Date("2015-05-29"),c("other_water_in.wash_plate_in")]<-1

m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("cont1.cont1_size")]<-154
m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("cont1.cont1_times")]<-1
m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("other_water_in.wash_clothes_in")]<-3
m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("other_water_in.wash_plate_in")]<-2
m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("other_water_in.adult_bathe_in")]<-2
m4[m4$HHID==055&m4$date_visit==as.Date("2014-12-17"),c("other_water_in.child_bathe_in")]<-1

m4[m4$HHID==301&m4$date_visit==as.Date("2015-01-03"),c("cont1.cont1_size")]<-38
m4[m4$HHID==301&m4$date_visit==as.Date("2015-01-03"),c("cont1.cont1_times")]<-1
m4[m4$HHID==301&m4$date_visit==as.Date("2015-01-03"),c("other_water_in.wash_clothes_in")]<-2
m4[m4$HHID==301&m4$date_visit==as.Date("2015-01-03"),c("other_water_in.wash_plate_in")]<-2
m4[m4$HHID==301&m4$date_visit==as.Date("2015-01-03"),c("other_water_in.adult_bathe_in")]<-2

#from Rebeca's clarifications
m4[m4$uniqueID=="338_2014-07-11",c("h2o_tank1")]<-0
m4[m4$uniqueID=="384_2014-07-12",c("h2o_distance1")]<-14.5

# fix data on  hours that water is available based on Bimal's changes to "water flow recheck" on 20-10-15
m4[m4$uniqueID=="020_2014-10-17"&m4$date_visit=="2015-02-20",c("checkwater")]<-6.5
m4[m4$uniqueID=="041_2015-05-20"&m4$date_visit=="2015-07-10",c("checkwater")]<-9
m4[m4$uniqueID=="233_2014-07-21",c("checkwater")]<-24
m4[m4$uniqueID=="400_2014-07-15"&m4$date_visit=="2015-05-08",c("checkwater")]<-24
m4[m4$uniqueID=="130_2014-11-18"&m4$date_visit=="2015-06-29",c("checkwater")]<-6.5
m4[m4$uniqueID=="134_2014-11-14"&m4$date_visit=="2015-05-25",c("checkwater")]<-6.5
m4[m4$uniqueID=="134_2014-11-14"&m4$date_visit=="2015-07-09",c("checkwater")]<-6.5
m4[m4$uniqueID=="218_2014-06-03"&m4$date_visit=="2015-02-27",c("checkwater")]<-6.5
m4[m4$uniqueID=="276_2014-08-22"&m4$date_visit=="2015-03-22",c("checkwater")]<-6.5
m4[m4$uniqueID=="279_2014-08-08"&m4$date_visit=="2015-07-24",c("checkwater")]<-6.5
m4[m4$uniqueID=="362_2014-07-14"&m4$date_visit=="2015-03-11",c("checkwater")]<-6.5
# from too many hours of water sheet
m4[m4$uniqueID=="026_2014-06-12"&m4$date_visit=="2015-03-06",c("checkwater")]<-24
m4[m4$uniqueID=="124_2014-12-15"&m4$date_visit=="2015-02-15",c("checkwater")]<-24
#delete entries that Bimal does not have on record and there are queries
m4<-m4[!(m4$uniqueID=="074_2014-09-05"&m4$date_visit=="2014-10-28"),] 
m4<-m4[!(m4$uniqueID=="187_2014-10-28"&m4$date_visit=="2015-08-27"),]
m4<-m4[!(m4$uniqueID=="200_2014-08-18"&m4$date_visit=="2014-11-09"),]
m4<-m4[!(m4$uniqueID=="263_2014-08-04"&m4$date_visit=="2014-10-23"),]
m4<-m4[!(m4$uniqueID=="391_2014-11-14"&m4$date_visit=="2015-07-09"),]

###data up until this point is cleaned on routine visits through 2015-10-01 #####

sub<-m4[m4$date_visit<"2015-10-01",]
table(sub$checkwater)
m4[m4$water_point1.wa_flow1.wa_time1.aS=="16:00:00"&m4$water_point1.wa_flow1.wa_time1.aE=="16:00:00"&m4$water_point1.wa_flow1.wa_time1.bS=="15:59:00",c("checkwater")]<-24 #FRA 9446 has entered 24 water use incorrectly
m4[m4$water_point1.wa_flow1.wa_time1.aS=="18:00:00"&m4$water_point1.wa_flow1.wa_time1.aE=="18:00:00"&m4$water_point1.wa_flow1.wa_time1.bS=="17:59:00"&m4$checkwater<1,c("checkwater")]<-24 #FRA 9446 has entered 24 water use incorrectly


# View(m4[m4$uniqueID=="078_2014-06-01",c("uniqueID","Listing.number.x","date_visit","FRA",
#                                         "water_point1.wa_flow1.wa_time1.aS","water_point1.wa_flow1.wa_time1.aE","water_flow_1",
#                                         "water_point1.wa_flow1.wa_time1.bS","water_point1.wa_flow1.wa_time1.bE","water_flow_2",
#                                         "water_point1.wa_flow1.wa_time1.cS","water_point1.wa_flow1.wa_time1.cE","water_flow_3",
#                                         "q15_recoded","checkwater")])

#insert values from rebeca's study here after checkwater water flow calculations
m4$clothes_water_per_wash <- ifelse(m4$checkwater>23,12.3,7)
m4$adult_bath_water_per_wash <- ifelse(m4$checkwater>23,34,39)
m4$child_bath_water_per_wash <- ifelse(m4$checkwater>23,16,9)
m4$dish_water_per_person <- ifelse(m4$checkwater>23,9,5)


#check for missing values
m4[is.na(m4$checkwater),c("uniqueID")]
m4[m4$uniqueID=="263_2014-08-04",c("checkwater")]

#after cleaning water use info, recaculate daily volume and per capita use


m4$daily_volume<-with(m4, (cont1.cont1_size*cont1.cont1_times)+(cont2.cont2_size*cont2.cont2_times)+
                        (cont3.cont3_size*cont3.cont3_times)+(cont4.cont4_size*cont4.cont4_times)+(cont5.cont5_size*cont5.cont5_times)
                      +(cont6.cont6_size*cont6.cont6_times)+(cont7.cont7_size*cont7.cont7_times)+(cont8.cont8_size*cont8.cont8_times)
                      +(cont9.cont9_size*cont9.cont9_times)+(cont10.cont10_size*cont10.cont10_times)+(cont11.cont11_size*cont11.cont11_times)
                      +(cont12.cont12_size*cont12.cont12_times)+(cont13.cont13_size*cont13.cont13_times)+(cont14.cont14_size*cont14.cont14_times)
                      +((other_water_in.adult_bathe_in+other_water_out.adult_bathe_out)*adult_bath_water_per_wash)  
                      +((other_water_out.child_bathe_out+other_water_in.child_bathe_in)*child_bath_water_per_wash)
                      +(ppl*dish_water_per_person)+(clothes*clothes_water_per_wash))

m4$daily_h2o_percapita<-with(m4, daily_volume/ppl)

mean(m4$daily_h2o_percapita)

#check entries with strange FRA IDs    
# table(m4$FRA)
# View(m4[m4$FRA==1333,])
# View(m4[m4$uniqueID=="157_2014-07-13",]) #### All are OK

# RE-CHECK CLEAN DATA -----------------------------------------------------



# SAVE --------------------------------------------------------------------

save(m4, file = data.output.path)

