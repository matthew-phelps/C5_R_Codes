# Cleaning baseline
# Output: Baseline file ready for comparing with X-1
# Matthew's pathnames:


#set working directory short cuts so they can be pasted together
wdmain<-"C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data"

# If Matthew, prepare Matthew's workingspace; else do nothing
ifelse(grepl("wrz741", getwd()), rm(list = ls()), NA)
ifelse(grepl("wrz741", getwd()), wdmain <- "C:\\Users\\wrz741\\Dropbox", NA)


wd1<-"\\C5 Baseline data\\Pre-double entry\\Set 1, 403 households\\2ndEntry" #correct folder name
wd47<-"\\C5 Baseline data\\Double-entered data\\Set 3 of 47 households"
wd69<-"\\C5 Baseline data\\Double-entered data\\Set 2 of 69 households"



library(memisc)
library(plyr)

#load baseline data sets
setwd(paste(wdmain,wd1,sep=""))
main<-as.data.set(spss.system.file('main_2ndentry with dates.sav'),stringsAsFactors=FALSE)#correct file name
HH_member<-as.data.set(spss.system.file('Q11_2ndEntry.sav'), stringsAsFactors=FALSE)#correct file name
water_use1<-as.data.set(spss.system.file('q13_18_2ndEntry.sav'), stringsAsFactors=FALSE)#correct file name
distance<-as.data.set(spss.system.file('Q45_46_2ndentry.sav'), stringsAsFactors=FALSE)#correct file name

setwd(paste(wdmain,wd69,sep=""))
main69<-as.data.set(spss.system.file('main_FinalEntry_69q.sav'),stringsAsFactors=FALSE) 
HH_member69<-as.data.set(spss.system.file('q11_FinalEntry_69q.sav'), stringsAsFactors=FALSE) 
water_use69<-as.data.set(spss.system.file('q13_18_FinalEntry_69q.sav'), stringsAsFactors=FALSE)
distance69<-as.data.set(spss.system.file('q45_46_FinalEntry_69q.sav'), stringsAsFactors=FALSE)#correct file name

setwd(paste(wdmain,wd47,sep=""))
main47<-as.data.set(spss.system.file('Final47_main.sav'),stringsAsFactors=FALSE)   
HH_member47<-as.data.set(spss.system.file('Final47_Q11.sav'), stringsAsFactors=FALSE) 
water_use47<-as.data.set(spss.system.file('Final47_Q13_18.sav'), stringsAsFactors=FALSE)
distance47<-as.data.set(spss.system.file('Final47_Q45_46.sav'), stringsAsFactors=FALSE)#correct file name

# make it easier to work with in R, convert to data frame
Q11 <-as.data.frame(HH_member) 
Q11_47<-as.data.frame(HH_member47) 
Q11_69<-as.data.frame(HH_member69) 
      
main<-as.data.frame(main)
main47<-as.data.frame(main47)
main69<-as.data.frame(main69)

water_use1<-data.frame(water_use1)
water_use47<-data.frame(water_use47)
water_use69<-data.frame(water_use69)

distance<-data.frame(distance)
distance69<-data.frame(distance69)
distance47<-data.frame(distance47)

#combine main baseline data sets
#first add missing columns
names1<-names(main)
names47<-names(main47)
names69<-names(main69)
names1[!(names1 %in% names47)]
names69[!(names69 %in% names47)] #nothing needs to be added to main47


names47[!(names47 %in% names1)]
names69[!(names69 %in% names1)]
main[,c("q55_oth", "q67_3_9",  "q67_3_10", "q67_4_12", "q67_4_13", "q67_4_14", "q67_4_15", "q67_5_6")]<-NA

names1[!(names1 %in% names69)]
names47[!(names47 %in% names69)]
main69[,c("q67_3_7",  "q67_3_8", "q67_4_11", "q67_3_9", "q67_3_10", "q67_4_12", "q67_4_13", "q67_4_14", "q67_4_15", "q67_5_6")]<-NA

##create readable dates
spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

main$intdate <- spss2date(main$intdate)
main47$intdate <- spss2date(main47$intdate)
main69$intdate <- spss2date(main69$intdate)#no dates entered yet, waiting for data entry

# Format HHID with leading 0s in order to make unique ID
main$hhid <- formatC(main$hhid, width = 3, format = 'd', flag = 0)
main47$hhid <- formatC(main47$hhid, width = 3, format = 'd', flag = 0)
main69$hhid <- formatC(main69$hhid, width = 3, format = 'd', flag = 0)

# Create unique id in main baseline files
main$uniqueID<-paste(main$hhid,"_",main$intdate,sep="")
main47$uniqueID<-paste(main47$hhid,"_",main47$intdate,sep="")
main69$uniqueID<-paste(main69$hhid,"_",main69$intdate,sep="")

# Return HHID to original formatting:
main$hhid <- as.numeric(main$hhid)
main47$hhid <- as.numeric(main47$hhid)
main69$hhid <- as.numeric(main69$hhid)


#create slno for main47 and main69, will come in helpful when merging datasets later
main47$slno<-as.numeric(paste(main47$hhid,".",47,sep=""))
main69$slno<-as.numeric(paste(main69$hhid,".",69,sep=""))

#merge baselines
baselineAll<-rbind(main,main69,main47)

#add unique id onto q11
Q11<-merge(Q11, main[,c("uniqueID", "slno")], by="slno")
Q11_47<-merge(Q11_47, main47[,c("uniqueID", "hhid")], by="hhid")
Q11_69<-merge(Q11_69, main69[,c("uniqueID", "hhid")], by="hhid")

#combine all Q11s
Q11_47$slno<-as.numeric(paste(Q11_47$hhid,".",47,sep=""))
Q11_69$slno<-as.numeric(paste(Q11_69$hhid,".",69,sep=""))
Q11_all<-rbind(Q11,Q11_47,Q11_69) # one 999, is adult, no need to change

#CHECKPOINT: make sure ages make sense
#duplicated(Q11_all)
#summary(Q11_all)
#Q11_all$q11_3[Q11_all$q11_4>=99]# one 999,unknown, but is adult, no need to change

# make dummy variable for each age range so counting will be easier
Q11_all$child_U5 <- ifelse(Q11_all$q11_4 < 5, 1, 0)
Q11_all$child_5_17 <- ifelse(Q11_all$q11_4 >= 5 & Q11_all$q11_4 <18, 1, 0)
Q11_all$adult <- ifelse(Q11_all$q11_4 >= 18, 1, 0)

# For each unique "slno" in df Q11_all, sum the number of adults, children U5 and children 5-17.
# Change "slno" to "hhid" if you wish to count based on HHID.
Q11subset <- ddply(Q11_all, .(slno),
                   summarize,
                   adults = sum(adult),
                   children_U5 = sum(child_U5),
                   children_5_17 = sum(child_5_17))

#merge age group subset with baseline data 
baselineAll<-merge(baselineAll,Q11subset,by="slno")

#View(baseline$children_U5)

# check to make sure summation works as expected. Returns "TRUE" if it works #first combine with baseline
sum(Q11_all$child_U5) == sum(baselineAll$children_U5)

#create total household members variable
baselineAll$total_HH_members<-baselineAll$children_5_17+baselineAll$children_U5+baselineAll$adults

#add unique id onto water use
water_use1<-merge(water_use1, main[,c("uniqueID", "slno")], by="slno", incomparables = NA)
water_use47<-merge(water_use47, main47[,c("uniqueID", "hhid")], by="hhid", incomparables = NA)
water_use69<-merge(water_use69, main69[,c("uniqueID", "hhid")], by="hhid", incomparables = NA)

#create water user group code
#go into spss files and search for "showar" in q14oth. Change corresponding q14 to 888
#water_use1$q14oth=="SHOWAR"# rows 296 and 297, change to 

#change rows 297 and 296 so shower becomes pipe
water_use1$q14<-ifelse(water_use1$q14oth=="SHOWAR",1,water_use1$q14)

#all 777 answers to q14 are now ground tank (with bucket)

#combine all water use data sets
water_use47$slno<-as.numeric(paste(water_use47$hhid,".",47, sep=""))
water_use69$slno<-as.numeric(paste(water_use69$hhid,".",69, sep=""))

water_useAll<-rbind(water_use1,water_use47,water_use69)



#recoding water sources
#use with()  for the following so i don't have to repeat datafram names$
#recode q14 so that 1 = pipe/tap, 2= hand pump, 3= well with bucket
water_useAll$q14_recoded<- with(water_useAll, ifelse(q14==1,1, ifelse(q14==2,1, ifelse(q14==3,2,                       
                               ifelse(q14==4,2, ifelse(q14==5,3, 
                               ifelse(q14==777,3, ifelse(q14==888, NA, 0))))))))

#recode 14a WASA=1; deep tube well/submersible =2,3 <-2 ; well/shallow tube well = 4,5 <-3; there are 2 777s w/o specification in dataset (both taps with tanks)
water_useAll$q14a_recoded<- as.numeric(ifelse(water_useAll$q14a==1,1, ifelse(water_useAll$q14a==2,2,                       
                                ifelse(water_useAll$q14a==3,2, ifelse(water_useAll$q14a==4,3, 
                                ifelse(water_useAll$q14a==5,3, ifelse(water_useAll$q14a==777, NA, 0)))))))

#recode q15 is there a tank? 1=yes, 0=no
water_useAll$q15_recoded<-as.numeric(ifelse(water_useAll$q15>=1,1,0))

#is a bucket needed to withdraw water? 1=yes, 0=no
water_useAll$use_bucket<-ifelse(water_useAll$q15==5|water_useAll$q14a==777|water_useAll$q14a==4,1,0)

#combine baseline and water_use
#Create new subset for primary water source
watersub<-water_useAll[,c("slno","use_bucket", "q15_recoded","q14a_recoded","q14_recoded","q13","q17")]
source1<-data.frame(watersub[which(watersub$q13==1),])
source2<-data.frame(watersub[which(watersub$q13==2),])
source3<-data.frame(watersub[which(watersub$q13==3),])
#rename columns on source 2 and 3 so they can be merged without confusion
names(source2)[2]<-paste("use_bucket2")
names(source3)[2]<-paste("use_bucket3")
names(source2)[3]<-paste("q15_recoded2")
names(source3)[3]<-paste("q15_recoded3")
names(source2)[4]<-paste("q14a_recoded2")
names(source3)[4]<-paste("q14a_recoded3")
names(source2)[5]<-paste("q14_recoded2")
names(source3)[5]<-paste("q14_recoded3")
names(source2)[6]<-paste("q13_2")
names(source3)[6]<-paste("q13_3")
names(source2)[7]<-paste("q17_2")
names(source3)[7]<-paste("q17_3")
#need same number of rows to cbind
source2[321:518,]<-0
source3[149:518,]<-0

watersources<-cbind(source1,source2,source3,by="slno", incomparables=NA)

#check for same slnos in all
watersources$slno[!(watersources$slno %in% baselineAll$slno)]

baselineAll <- cbind(baselineAll, watersources, by="slno") 

#####Distance to primary water source#######
distance47$slno<-as.numeric(paste(distance47$hhid,".",47, sep=""))
distance69$slno<-as.numeric(paste(distance69$hhid,".",69, sep=""))
dall<-rbind(distance,distance47,distance69)

dall$q45<-as.numeric(dall$q45)
distance_to_source1<-dall$q46_1[dall$q45==1]

baselineAll<-cbind(baselineAll,distance_to_source1)
baselineAll$distance_to_source1[is.na(baselineAll$distance_to_source1)]<-21 #all NAs were GPS locations, taken because distance was more than 20 meters



# Move Unique ID to front column and save dataset -------------------------
y <- match(c('uniqueID'), names(baselineAll))
x <- 1:(ncol(baselineAll) - length(y))
baselineAll <- baselineAll[, c(y, x)]
rm(x, y)
save(baselineAll, file = "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baselineAll.Rdata")

#----------------------------------------------------------------------------

# if desired storage capacity need to do for baseline data sets 1 and 2
#water_storage<-as.data.set(spss.system.file('Q24_Q30.sav'))
#ddply(water_storage, c("q26"),
#       summarise,
#       count = sum(q26[hhid==hhid]))
#create calculation for total storage capacity (volume*number of containers); this may have been done already in a separate file
#if needed q19_drinkwater<-as.data.set(spss.system.file('q19.sav'))
#ddply(q19_drinkwater,c("hhid"),
#       summarise,
#       count = length(hhid[q19==19.1 & q19_1==1]))


#count duplicates




#variables to include
#Q6 rooms in house
#Q9_1 electricity
#Q10 by whom is the house occupied, 1=nuclear family, 2=multiple families, 
         #3=unrelated persons, 4= nuclear family with one or more related persons, 777=other
#12 income, use other R file for income calculation

#see "R script for wealth quintiles and water use by quintiles"

