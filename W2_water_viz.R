# Author: Matthew Phelps & Char Tamason
# Desc: Visualizing water usage data
# Dependencies: "Baseline descriptives.R"

# Load data ---------------------------------------------------------------
rm(list = ls())
ifelse(grepl("zrc340", getwd()), 
       water.usage.path<- "C:\\Users\\zrc340\\Desktop\\Dropbox\\Cholera PhD\\5C\\Analysis\\C5_R_Codes\\Rdata\\monthly-water.Rdata",
       water.usage.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-water.Rdata")



# LOAD DATA ---------------------------------------------------------------

load(water.usage.path)

monthly$daily_h2o_percapita[1]
