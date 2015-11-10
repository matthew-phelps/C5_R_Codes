# Author: Matthew Phelps & Char Tamason
# Desc: Visualizing water usage data
# Dependencies: "Baseline descriptives.R"

# Load data ---------------------------------------------------------------
rm(list = ls())
graphics.off()
ifelse(grepl("zrc340", getwd()), 
       water.usage.path<- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata",
       water.usage.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-water.Rdata")

library(ggplot2)

# LOAD DATA ---------------------------------------------------------------

load(water.usage.path)



# Variables needed --------------------------------------------------------

monthly<-m4

monthly$water_access_group <- ifelse(monthly$h2o_distance1>=10&monthly$h2o_collect1>1&monthly$checkwater<24,1,
                                     ifelse(monthly$h2o_distance1>=10&monthly$h2o_collect1==1&monthly$checkwater<24,2,
                                            ifelse(monthly$h2o_distance1>=10&monthly$h2o_collect1>1&monthly$checkwater==24,3,
                                                   ifelse(monthly$h2o_distance1>=10&monthly$h2o_collect1==1&monthly$checkwater==24,4,
                                                   ifelse(monthly$h2o_distance1<10&monthly$h2o_collect1>1&monthly$checkwater<24,5,
                                                   ifelse(monthly$h2o_distance1<10&monthly$h2o_distance1>0&monthly$h2o_collect1==1&monthly$checkwater<24,6,
                                                          ifelse(monthly$h2o_distance1<10&monthly$h2o_collect1>1&monthly$checkwater==24,7,
                                                                 ifelse(monthly$h2o_distance1>0&monthly$h2o_distance1<10&monthly$h2o_collect1==1&monthly$checkwater==24,8,
                                                                        ifelse(monthly$h2o_distance1==0&monthly$h2o_collect1==1&monthly$checkwater==24,9,
                                                                               ifelse(monthly$h2o_distance1==0&monthly$h2o_collect1==1&monthly$checkwater<24,10,11
                                                                               ))))))))))
length(which(monthly$water_access_group==1))
length(which(monthly$water_access_group==2))
length(which(monthly$water_access_group==3))
length(which(monthly$water_access_group==4))
length(which(monthly$water_access_group==5))
length(which(monthly$water_access_group==6))
length(which(monthly$water_access_group==7))
length(which(monthly$water_access_group==8))
length(which(monthly$water_access_group==9))
length(which(monthly$water_access_group==10))


monthly[monthly$distance_to_source1==0,"h2o_collect1"]

# PLOTS -----------------------------------------------------------------

boxplot(daily_h2o_percapita~month, data=monthly)

boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 1, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 2, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 3, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 4, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 5, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 6, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 7, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 8, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 9, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 10, ])

mean(monthly$daily_h2o_percapita)

# Plot a subset of unique ids

sub <- m4[m4$slno.1>100&m4$slno.1<200,]# assuming random assingment of numbers (which it was; there was no logical system to this)


#plotting based on month doesn't work because we started in 2014. Create year.month variable to get dates in the correct order


sub$year.month<-as.numeric(with(sub, ifelse(month=="09"|month=="10"|month=="11"|month=="12",paste("14.",month,sep=""),paste("15.",month,sep=""))))
sub<-sub[order(sub$year.month),]

head(sub$year.month)
nuniqueid<-max(sub$slno.1) #create number of lines, slno is a unique number created during baseline phase to merge baselines and is numeric
xrange<-range(sub$date_visit)
yrange<-range(sub$daily_h2o_percapita)

plot(daily_h2o_percapita~date_visit,data = sub, xlab="Month",ylab="Daily water consumption per capita" )


# GGPLOT ------------------------------------------------------------------

# Use ggplot function in the 'ggplot2' package 
ggplot(data = sub, aes(x = date_visit, y = daily_h2o_percapita)) +
  geom_line(aes( color = uniqueID))

ggplot(data=sub, aes(x=date_visit, y= clothes)) + geom_line(aes( color = uniqueID))

ggplot(data=sub, aes(x=date_visit, y= dishes)) + geom_line(aes( color = uniqueID))

