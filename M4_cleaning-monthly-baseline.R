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
m4$ppl<-ifelse(m4$ppl==0,m4$total_HH_members,m4$ppl)
m4$daily_h2o_percapita<-with(m4, daily_volume/ppl)

#If the range of water consumption per capita is > 50 LCPD, flag for follow up

# for i in unique(m4$uniqueID) {
#   
# }


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





# RE-CHECK CLEAN DATA -----------------------------------------------------



# SAVE --------------------------------------------------------------------

save(m4, file = data.output.path)

