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

monthly$h2o_tank1

# PLOTS -----------------------------------------------------------------

boxplot(daily_h2o_percapita~month, data=monthly)

boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$base_tank1 == 1, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$base_tank1 == 0, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 3, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 5, ])

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

