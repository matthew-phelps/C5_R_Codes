## Run "creating a joint baseline file" first

setwd("C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Baseline data")
library(memisc)
library(plyr)

#quintiles
#set NAs to 0 in q12d How much are you paying per month in loans
baselineAll$q12d[is.na(baselineAll$q12d)]<-0

#monthly income = average monthly household income + monthly remittances received - monthly remittances sent + annual remittances received/12 - annual remittances sent/12 - monthly loan payment
baselineAll$Monthly_income<- baselineAll$q12 + baselineAll$q12a2 - baselineAll$q12a1 + 
  (baselineAll$q12a3/12)-(baselineAll$q12a4/12)- baselineAll$q12d

baselineAll$monthly_income_percapita<-baselineAll$Monthly_income/(baselineAll$total_HH_members)

#View(baselineAll$monthly_income_percapita)

#shared facilities (water q17, kitchen q31, latrines q35) 0=all shared, 1=2of3 shared, 2= 1of3 shared
baselineAll$shared_facilities<- with(baselineAll, ifelse(q17==1& q31 >=1 & q35==1, 0, 
                                                       ifelse(q17==0& q31 >=1 & q35==1, 1,
                                                       ifelse(q17==1& q31 >=1 & q35==0, 1,
                                                       ifelse(q17==1& q31 ==0 & q35==1, 1,
                                                       ifelse(q17==0& q31 >=1 & q35==0, 2,
                                                       ifelse(q17==0& q31 ==0 & q35==1, 2,
                                                       ifelse(q17==1& q31 ==0 & q35==0, 2, NA))))))))

#table(baselineAll$shared_facilities)

#create column with income quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
baselineAll$income_quintile<-as.integer(cut(baselineAll$Monthly_income, quantile(baselineAll$Monthly_income, 
                                                                           probs=0:5/5, include.lowest=TRUE)))
  
#asset calculation
#in q9_20_oth1, q9_20_oth2, q9_20_oth3 meatshelf (cupboard), dressing table (dresser), and sofa should earn 1 point, rickshaw should earn 2, 
# names(baselineAll$q9_20_oth1)
# baselineAll$q9_20_oth1<-as.character(baselineAll$q9_20_oth1)
# 
# x<-grep("^M",baselineAll$q9_20_oth1)
# baselineAll$q9_20_oth1[baselineAll$q9_20_oth1==x,]<-"MEAT SHELF"
# 
# baselineAll$q9_20_oth1[baselineAll$q9_20_oth1==grep("^M",baselineAll$q9_20_oth1)]<-"MEAT SHELF"
# 
# baselineAll$q9_20_oth1p <- with(baselineAll, ifelse(q9_20_oth1=="COMPUTER",3,
#                                                     ifelse(q9_20_oth1=="VEDEO CAMERA",2,0)))
#                                                     ifelse(q9_20_oth1=="DRESIN TABLE",1,
#                                                     ifelse(q9_20_oth1=="DRESING TABLE  (1)",1,
#                                                     ifelse(q9_20_oth1=="DREsSING TABLE",1,
#                                                     ifelse(q9_20_oth1=="DRESSING TABLE (01)",1,
#                                                     ifelse(q9_20_oth1=="DRESSING TABLE.",1,
#                                                     ifelse(q9_20_oth1=="DREASSING TABLE",1,
#                                                     ifelse(q9_20_oth1=="EATSSAFE",1,
#                                                     ifelse(q9_20_oth1=="SOFA",1,
#                                                     ifelse(q9_20_oth1=="SOFA SET",1,
#                                                     ifelse(q9_20_oth1=="RICKSHAW",2,
#                                            
#                                             )))))))))
# )
#########repeat with q9_20_oth2 and q9_20_oth13#######

                                            
                               
table(baselineAll$q9_20_oth1)
#count assets
baselineAll$asset_score<- (as.numeric(baselineAll$q9_1) + as.numeric(baselineAll$q9_2) + as.numeric(baselineAll$q9_3) +
                          as.numeric(baselineAll$q9_5) + as.numeric(baselineAll$q9_6) + as.numeric(baselineAll$q9_7) + as.numeric(baselineAll$q9_8) +
                          as.numeric(baselineAll$q9_9) + as.numeric(baselineAll$q9_15) + as.numeric(baselineAll$q9_16) +
                          as.numeric(baselineAll$q9_18)+ ((as.numeric(baselineAll$q9_10) +
                          (as.numeric(baselineAll$q9_11) + as.numeric(baselineAll$q9_12))*2) + 
                          ((as.numeric(baselineAll$q9_13) + as.numeric(baselineAll$q9_19))*3)+ baselineAll$q9_20_oth1p
                          +(baselineAll$shared_facilities)*3))

#View(baselineAll$asset_score)
baselineAll$asset_score[baselineAll$asset_score==NA]
#create column with asset quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
is.na(baselineAll$asset_score)
baselineAll$asset_quintile<-as.integer(cut(baselineAll$asset_score, quantile(baselineAll$asset_score, 
                                                                       probs=0:5/5, include.lowest=TRUE)))
table(baselineAll$asset_quintile)
View(baselineAll[184,])

#create quintile variable called income_quintile within baseline
baselineAll$income_quintile <- with(baselineAll, as.integer(cut(Monthly_income, 
                    quantile(Monthly_income, probs=0:5/5), include.lowest=TRUE)))




#water source by quintile
ddply(main_w_water_use, c("q14a"),
      summarise,
      count = length(q14a[q13==1 & quintile==1]), length(q14a[q13==1 & quintile==2]),length(q14a[q13==1 & quintile==3]),
      length(q14a[q13==1 & quintile==4]),
      length(q14a[q13==1 & quintile==5]))



