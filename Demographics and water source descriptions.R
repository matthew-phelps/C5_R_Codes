#computing monthly income
setwd("C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Baseline data")
#use package memisc to import spss files#
library(memisc)
baseline<-as.data.set(spss.system.file('main2.sav'), stringsAsFactors=FALSE)
baseline$q12d[is.na(baseline$q12d)]<-0
View(baseline$q12d)
baseline$Monthly_income<- baseline$q12 + baseline$q12a2 - baseline$q12a1 + 
  (baseline$q12a3/12)-(baseline$q12a4/12)- baseline$q12d
View(baseline$Monthly_income)
#computing quintiles
quantile(main$Monthly_income, c(.2,.4,.6,.8,1))

#Water user groups, use package plyr
setwd("C:/Users/zrc340/Desktop/Dropbox/C5 data/C5 Baseline data")
water_use<-as.data.set(spss.system.file('Q13_18.sav'), stringsAsFactors=FALSE)
library(plyr)
#source where most water is taken Q13==1
#count access points for source where most water is taken WASA
#WASA 14A==1
ddply(water_use,c("q14"),
      summarise,
      count = length(q14[q13 == 1 & q14a==1]))
#tank for storage for source where most water is taken WASA
## 0 = no tank
ddply(water_use,c("q15"),
      summarise,
      count = length(q15[q13 == 1 & q14a==1]))
#count access points for source where most water is taken submersible
#Submersible 14a==2 individual, ==3 communal
ddply(water_use,c("q14"),
      summarise,
      count = length(q14[q13 == 1 & q14a==2]))
ddply(water_use,c("q14"),
      summarise,
      count = length(q14[q13 == 1 & q14a==3]))
#tank for storage for source where most water is taken submersible
ddply(water_use,c("q15"),
      summarise,
      count = length(q15[q13 == 1 & q14a==2]))
ddply(water_use,c("q15"),
      summarise,
      count = length(q15[q13 == 1 & q14a==3]))
#Shallow tube well 14a==5
ddply(water_use,c("q14"),
      summarise,
      count = length(q14[q13 == 1 & q14a==5]))
#tank for storage for source where most water is taken shallow tube well
## 0 = no tank
ddply(water_use,c("q15"),
      summarise,
      count = length(q15[q13 == 1 & q14a==5]))
#how many children
q11_HH_mem<-as.data.set(spss.system.file('Q11.sav'))
range(q11_HH_mem$idno)
ddply(q11_HH_mem,c("idno"),
      summarise,
      count = length(idno[q11_4<18]))
#occupations 
##q11_6 is occupations, Q11_4 is age and q11_3 is sex, where m=1 f=2
ddply(q11_HH_mem,c("q11_6"),
      summarise,
      count = length(q11_6[q11_4>=18 & q11_3==1]))
ddply(q11_HH_mem,c("q11_6"),
      summarise,
      count = length(q11_6[q11_4>=18 & q11_3==2]))
#literacy
ddply(q11_HH_mem,c("q11_7"),
      summarise,
      count = length(q11_7[q11_4>=18 & q11_3==1]))
ddply(q11_HH_mem,c("q11_7"),
      summarise,
      count = length(q11_7[q11_4>=18 & q11_3==2]))
