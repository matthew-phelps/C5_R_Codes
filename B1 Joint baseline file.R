# Cleaning baseline
# Output: Baseline file ready for comparing with X-1
# Dependencies: None



# Intro -------------------------------------------------------------------

# Prepare Matthew's workspace if user == MATTHEW. If else, setwd to Chars dir
rm(list = ls())
ifelse(grepl("zrc340", getwd()), 
       data.output.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\baselineAll.Rdata",
       data.output.path <- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baselineAll.Rdata")
ifelse(grepl("zrc340", getwd()), 
       data.output.path2<- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\Q11_all.Rdata",
       data.output.path2<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Q11_all.Rdata")

#set working directory short cuts so they can be pasted together
ifelse(grepl("zrc340", getwd()), 
       wdmain <- "C:\\Users\\zrc340\\Desktop\\Dropbox\\C5 data",
       wdmain <- "C:\\Users\\wrz741\\Dropbox")

wd1<-"\\C5 Baseline data\\Double-entered data\\Set 1 of 403 households\\FinalEntry"
wd47<-"\\C5 Baseline data\\Double-entered data\\Set 3 of 47 households"
wd69<-"\\C5 Baseline data\\Double-entered data\\Set 2 of 69 households"

library(memisc)
library(plyr)

#load baseline data sets
setwd(paste(wdmain,wd1,sep=""))
main<-as.data.set(spss.system.file('main_FinalEntry.sav'),stringsAsFactors=FALSE)#correct file name
HH_member<-as.data.set(spss.system.file('Q11_FinalEntry.sav'), stringsAsFactors=FALSE)#correct file name
water_use1<-as.data.set(spss.system.file('q13_18_FinalEntry.sav'), stringsAsFactors=FALSE)#correct file name
distance<-as.data.set(spss.system.file('Q45_46_FinalEntry.sav'), stringsAsFactors=FALSE)#correct file name

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


# FACTORS TO STRINGS: ---------------------------------------------------
# Replace all variables that are class: factors with variabels tha 

# see: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
main1 <- rapply(main, as.character, classes="factor", how="replace")
main <- as.data.frame(main1, stringsAsFactors = F) # get back to nice df

main2 <- rapply(main47, as.character, classes="factor", how="replace")
main47 <- as.data.frame(main2, stringsAsFactors = F) 

main3 <- rapply(main69, as.character, classes="factor", how="replace")
main69 <- as.data.frame(main3, stringsAsFactors = F) 

rm(main1, main2, main3)
  
# COMBINE DATASETS --------------------------------------------------------

#first add missing columns
names1<-names(main)
names47<-names(main47)
names69<-names(main69)
names1[!(names1 %in% names47)]
names69[!(names69 %in% names47)] #nothing needs to be added to main47




names47[!(names47 %in% names1)]
names69[!(names69 %in% names1)]
main[,c("entdate","q55_oth", "q67_3_9",  "q67_3_10", "q67_4_12", "q67_4_13", "q67_4_14", "q67_4_15", "q67_5_6")]<-NA

names1[!(names1 %in% names69)]
names47[!(names47 %in% names69)]
main69[,c("q67_3_7",  "q67_3_8", "q67_4_11", "q67_3_9", "q67_3_10", "q67_4_12", "q67_4_13", "q67_4_14", "q67_4_15", "q67_5_6")]<-NA

##create readable dates
spss2date <- function(x)  x + ISOdate(1582,10,14) 

main$intdate <- spss2date(main$intdate)
main47$intdate <- spss2date(main47$intdate)
main69$intdate <- spss2date(main69$intdate)


#create slno for main47 and main69, will come in helpful when merging datasets later
main47$slno<-as.numeric(paste(main47$hhid,".",47,sep=""))
main69$slno<-as.numeric(paste(main69$hhid,".",69,sep=""))

#merge baselines
baselineAll<-rbind(main,main69,main47)

#fix unique ids, remove the hour:minute:second from spss2date function
baselineAll$intdate<-as.character(baselineAll$intdate)
temp<-strsplit(baselineAll$intdate, " ")
mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("base_date", "_")
baselineAll<-cbind(df[c("base_date")],baselineAll)
baselineAll$base_date <- as.Date(baselineAll$base_date)
baselineAll$base_date
# Format HHID with leading 0s in order to make unique ID
baselineAll$hhid <- formatC(baselineAll$hhid, width = 3, format = 'd', flag = 0)

# Create unique id in main baseline files
baselineAll$uniqueID<-paste(baselineAll$hhid,"_",baselineAll$base_date,sep="")

# Return HHID to original formatting:
baselineAll$hhid <- as.numeric(baselineAll$hhid)


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

#create Q11 data so it can be used in descriptive analysis 
save(Q11_all, file = data.output.path2)

# For each unique "slno" in df Q11_all, sum the number of adults, children U5 and children 5-17.
# Change "slno" to "hhid" if you wish to count based on HHID.
Q11subset <- ddply(Q11_all, .(slno),
                   summarize,
                   adults = sum(adult),
                   children_U5 = sum(child_U5),
                   children_5_17 = sum(child_5_17),
                   ppl_all = sum(adult) + sum(child_U5) + sum(child_5_17))

#merge age group subset with baseline data 
baselineAll<-merge(baselineAll,Q11subset,by="slno")


#View(baseline$children_U5)

# check to make sure summation works as expected. Returns "TRUE" if it works #first combine with baseline
sum(Q11_all$child_U5) == sum(baselineAll$children_U5)

#create total household members variable
baselineAll$total_HH_members<-baselineAll$children_5_17+baselineAll$children_U5+baselineAll$adults

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
#names(watersub)
#combine baseline and water_use
#Create new subset for primary water source
watersub<-water_useAll[,c("slno","use_bucket", "q15_recoded","q14a_recoded","q14_recoded","q13","q17","q17_1","q17_2")]
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
names(source2)[7]<-paste("q17.2")
names(source3)[7]<-paste("q17.3")
names(source2)[8]<-paste("q17_2.2")
names(source3)[8]<-paste("q17_2.3")
names(source2)[9]<-paste("q17_3.2")
names(source3)[9]<-paste("q17_3.3")

watersources<-merge(source1,source2,by="slno", all=T)
watersources<-merge(watersources,source3, by="slno",all=T)

#check for same slnos in all
watersources$slno[!(watersources$slno %in% baselineAll$slno)]

baselineAll <- merge(baselineAll, watersources, by="slno") 

#####Distance to primary water source#######
distance47$slno<-as.numeric(paste(distance47$hhid,".",47, sep=""))
distance69$slno<-as.numeric(paste(distance69$hhid,".",69, sep=""))
dall<-rbind(distance,distance47,distance69)

dall$q45<-as.numeric(dall$q45)
dall$q46_1[is.na(dall$q46_1)]<-21 #all NAs were GPS locations, taken because distance was more than 20 meters


distance_to_source1<-dall[dall$q45==1, c("q46_1","slno")]
distance_to_source1<-as.data.frame(distance_to_source1)
distance_to_source2<-dall[dall$q45==2,c("q46_1","slno")]
distance_to_source2<-as.data.frame(distance_to_source2)

distance<-merge(distance_to_source1,distance_to_source2,by="slno",all=T)
colnames(distance)[2]<-"distance_to_source1"
colnames(distance)[3]<-"distance_to_source2"

baselineAll<-merge(baselineAll,distance, by.x="slno",by.y="slno")
baselineAll$distance_to_source1.y<-NULL
baselineAll$distance_to_source2.y<-NULL
baselineAll[baselineAll$listing==1089,c("uniqueID", "q14_recoded","q15_recoded","distance_to_source1")]
# Move Unique ID to front column -------------------------
y <- match(c('uniqueID', 'hhid'), names(baselineAll))
x <- 1:ncol(baselineAll)
x <- x[-c(y)]
baselineAll <- baselineAll[, c(y, x)]
rm(x, y)



# SAVE --------------------------------------------------------------------
save(baselineAll, file = data.output.path)


