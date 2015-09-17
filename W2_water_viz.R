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

nrow(monthly[monthly$water_access_group == 1, ])
nrow(monthly[monthly$water_access_group == 2, ])
nrow(monthly[monthly$water_access_group == 3, ])
nrow(monthly[monthly$water_access_group == 5, ])


monthly$daily_h2o_percapita[1]
monthly$water_access_group2

box