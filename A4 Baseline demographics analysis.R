#Descriptive statistics on Baseline data only

library(chron)
library(lme4)
library(plyr)

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
#monthly[is.na(monthly)]<-0

#monthlysub<-monthly[!duplicated(monthly$uniqueID),] #subset of unique household ids first visit


# variables that will be used ---------------------------------------------

Q11subset<-Q11_all[Q11_all$q11_4>=18,] #only adults
Q11sub<- ddply(Q11subset, .(slno),
                  summarize,
                  literacy = sum(q11_7),
                  slno= unique(slno))

monthly$slno<-monthly$slno.1
monthly<-merge(monthly, Q11sub, by="slno", all.x = T, all.y = F )

             
#create month variable
monthly$date_visit_character<-as.character(monthly$date_visit)
temp<-strsplit(monthly$date_visit_character, "-")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("year", "month", "day")
monthly<- cbind(monthly,df)
monthly$month1<-formatC(monthly$month,width=2,format='d', flag = 0)

monthly$year.month<-as.numeric(with(monthly, ifelse(month1=="09"|month1=="10"|month1=="11"|
                                                      month1=="12",paste("14.",month,sep=""),paste("15.",month1,sep=""))))

monthly$season<-with(monthly, ifelse(month1=="01"|month1=="02"|month1=="03",1,
                                     ifelse(month1=="04"|month1=="05"|month1=="06",2,
                                            ifelse(month1=="07"|month1=="08"|month1=="09",3,4))))


monthly$ppl[is.na(monthly$ppl)]<-0
monthly$ppl<-ifelse(monthly$ppl==0,monthly$total_HH_members,monthly$ppl)
monthly$daily_h2o_percapita<-with(monthly, daily_volume/ppl)



# Household size at baseline----------------------------------------------------------

monthly$children<-monthly$children_U5+monthly$children_5_17
monthly$total_hh_members<- monthly$children + monthly$adult
uniqueid<-unique(monthly$uniqueID)


range(mothly$total_HH_members)
mean(mothly$total_HH_members)
range(mothly$adults)
mean(mothly$adults)
range(mothly$children_5_17)
mean(mothly$children_5_17)
range(mothly$children_U5)
mean(mothly$children_U5)


# Household structure -----------------------------------------------------

#q10 nuclear =1, multiple families =2, unrelated persons = 3, nuclear family with 1+ unrelated = 4, other=777
#table(monthly$q10oth)
monthly[monthly$q10oth=="SINGLE","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE.","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE (MESS)","q10"]<-3 #single is mess hall, same as unrelated people
monthly[monthly$q10oth=="SINGLE PERSON","q10"]<-3 #single is mess hall, same as unrelated people
monthly[grep("^SINGLE MEMBER,", monthly$q10oth),"q10"]<-4 #this is one single person living with nuclear family
monthly[grep("^ONE", monthly$q10oth),"q10"]<-4 #this is one single person living with nuclear family
monthly[grep("^ONLY", monthly$q10oth),"q10"]<-1 #two sisters living together
#monthly$q10oth[monthly$q10==777]

monthly$nuclear_family<- with(monthly, ifelse(q10==1|q10==2|q10==4,1,0)) #is it one or more nuclear families (1) or primarily unrelated people (0)?

monthlysub<-monthly[!duplicated(monthly$uniqueID),]
table(monthlysub$nuclear_family)


# literacy ----------------------------------------------------------------
monthly$literacy_coded <- ifelse(monthly$adults==monthly$literacy,3,
                                 ifelse(monthly$literacy==0,1,2))

monthlysub<-monthly[!duplicated(monthly$uniqueID),]
table(monthlysub$literacy_coded)

# Household monthly income ------------------------------------------------
#monthly income = average monthly household income + monthly remittances received - monthly remittances sent + annual remittances received/12 - annual remittances sent/12 - monthly loan payment

monthly$q12[is.na(monthly$q12)]<-0 #set NAs to 0 for income questions 
monthly$q12a2[is.na(monthly$q12a2)]<-0 #
monthly$q12a1[is.na(monthly$q12a1)]<-0 #
monthly$q12a3[is.na(monthly$q12a3)]<-0 #
monthly$q12a4[is.na(monthly$q12a4)]<-0 #
monthly$q12d[is.na(monthly$q12d)]<-0

monthly$Monthly_income<- monthly$q12 + monthly$q12a2 - monthly$q12a1 + 
  (monthly$q12a3/12)-(monthly$q12a4/12)- monthly$q12d

monthly$monthly_income_percapita<-monthly$Monthly_income/(monthly$ppl)

monthlysub<-monthly[!duplicated(monthly$uniqueID),]

#View(monthly[monthly$monthly_income_percapita>20000,c("monthly_income_percapita","ppl",
                                                            "asset_score","shared_facilities")])




# View(monthly$monthly_income_percapita)
#create column with income quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)

ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(monthly$monthly_income_percapita, probs = seq(0, 1, by = 0.20))), 
      include.lowest=TRUE)
}
mothly$income_Quintile <- sapply(mothly$monthly_income_percapita, ApplyQuintiles)

table(mothly$income_Quintile)

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

monthlysub<-monthly[!duplicated(monthly$uniqueID),]

ApplyQuintiles2 <- function(x) {
  cut(x, breaks=c(quantile(monthly$asset_score, probs = seq(0, 1, by = 0.20))), 
      include.lowest=TRUE)
}
mothly$asset_Quintile <- sapply(mothly$asset_score, ApplyQuintiles2)

table(mothly$asset_Quintile)

# Occupation --------------------------------------------------------------
###q11_6 employment: unemployed/student/retired=6, Un-skilled labor = 1, skilled labor = 2, Garments =3, salaried job = 4, spiritual healer=5, other = 777

uniqueslno<-unique(monthly$slno.1)


#table(Q11_all$q11_6)
Q11_all$jobs<-with(Q11_all, ifelse(q11_6==0,NA,
                                   ifelse(q11_6==1|q11_6==3|q11_6==4|q11_6==9|q11_6==11|q11_6==33,1,
                                          ifelse(q11_6==2|q11_6==29|q11_6==30|q11_6==31|q11_6==32,6,
                                                 ifelse(q11_6==5|q11_6==16|q11_6==17|q11_6==18|q11_6==19,4,
                                                        ifelse(q11_6==6,3,
                                                               ifelse(q11_6==7|q11_6==8|q11_6==15|q11_6==20|q11_6==21|q11_6==24,2,
                                                                      ifelse(q11_6==26,5,q11_6))))))))
sub<-Q11_all[Q11_all$q11_4>=18,] #look only at adults
table(sub$jobs)

q11_sub<-Q11_all[Q11_all$q11_5==1,] #household head

Q11_sub<-q11_sub[(q11_sub$slno==uniqueslno),]


# Primary water source info -------------------------------------------------------

#is there a tank
table(monthly$q15_recoded) # tank 1= yes, 0= no
#what is the infrastructure at point of collection
table(monthly$q14_recoded) # 1= tap 2= handpump 3= well with bucket
#what is the water source
table(monthly$q14a_recoded) # 1= WASA 2= deep tubewell 3= groundwater

#how far away is the collection point from the front door
monthly$distance<-with(monthly, ifelse(distance_to_source1==0,1,
                                       ifelse(distance_to_source1>0&distance_to_source1<=10,2,
                                              ifelse(distance_to_source1>10&distance_to_source1<=20,3,4))))
table(monthly$distance)

#how many water sources are used
table(monthly$q13_3) # 3= 3 or more sources
table(monthly$q13_2) # 2 - value from above = 2 sources
table(monthly$q13) # 1 - (value 2-value 3) = only 1 source

#hours per day that water is flowing from primary source
mean(monthly$checkwater)
range(monthly$checkwater)
table(monthly$checkwater)
