# Author: Matthew Phelps & Char Tamason
# Desc: Visualizing water usage data
# Dependencies: "Baseline descriptives.R"

# Load data ---------------------------------------------------------------
rm(list = ls())
graphics.off()
ifelse(grepl("zrc340", getwd()), 
       water.usage.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\monthly-water.Rdata",
       water.usage.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-water.Rdata")



# LOAD DATA ---------------------------------------------------------------

load(water.usage.path)




# PLOTS -----------------------------------------------------------------

boxplot(daily_h2o_percapita~month, data=monthly)

boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 1, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 2, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 3, ])
boxplot(daily_h2o_percapita ~ month, data = monthly[monthly$water_access_group == 5, ])



# Plot a subset of unique ids
sub<-monthly[monthly$daily_h2o_percapita<250&monthly$daily_h2o_percapita>0,] #get rid of outliers 
sub <- sub[sub$slno.1>100&sub$slno.1<250,]# assuming random assingment of numbers (which it was; there was no logical system to this)


#plotting based on month doesn't work because we started in 2014. Create year.month variable to get dates in the correct order
sub$month<-formatC(sub$month,width=2,format='d', flag = 0)
sub$year.month<-as.numeric(with(sub, ifelse(month=="09"|month=="10"|month=="11"|month=="12",paste("14.",month,sep=""),paste("15.",month,sep=""))))
sub<-sub[order(sub$year.month),]

head(sub$year.month)
nuniqueid<-max(sub$slno.1) #create number of lines, slno is a unique number created during baseline phase to merge baselines and is numeric
xrange<-range(sub$year.month)
yrange<-range(sub$daily_h2o_percapita)

plot(daily_h2o_percapita~date_visit,data = sub, xlab="Month",ylab="Daily water consumption per capita" )
colors<-rainbow(nuniqueid)
linetype<- c(1:nuniqueid)


#create lines
for (i in 1:nuniqueid) { 
  uniqueID <- subset(sub, nuniqueid==i) 
  lines(sub$year.month, sub$daily_h2o_percapita, type="l",
        lty=linetype[i]) 
}





