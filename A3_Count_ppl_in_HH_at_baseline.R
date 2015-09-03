# Author: Matthew Phelps
#Desc: Calculate number of people living in each household at baseline

# Intro -------------------------------------------------------------------

rm(list = ls())
graphics.off()
mac <- ""
pc <- "C:/Users/wrz741/Dropbox/C5 Baseline data/Pre-double entry"
setwd(pc)
rm(mac, pc)

library(plyr)
library(memisc) # read .spss files


HH_member_info<-as.data.set(spss.system.file('Q11.sav'), stringsAsFactors=FALSE)

# make it easier to work with in R. 
# Please change "x" to a variable name that makes sense for you!
x1 <-as.data.frame(HH_member_info) 
x1$hhid <-formatC(x1$hhid, width = 3, format = 'd', flag = 0)

# make dummy variable for each age range so counting will be easier
x1$child_U5 <- ifelse(x1$q11_4 < 5, 1, 0)
x1$child_5_17 <- ifelse(x1$q11_4 >= 5 & x1$q11_4 <18, 1, 0)
x1$adult <- ifelse(x1$q11_4 >= 18, 1, 0)

# For each unique "slno" in df "x", sum the number of adults, children U5 and children 5-17.
# Change "slno" to "hhid" if you wish to count based on HHID. This gives 1 record for each house
baseline_ppl <- ddply(x1, .(hhid),
      summarize,
      adults = sum(adult),
      children_U5 = sum(child_U5),
      children_5_17 = sum(child_5_17),
      all_ppl = adults + children_U5 + children_5_17)

# Add HHID variable to make our final dataset. This gives one record for each person.
# x3 <- merge(x[,c(1,10)], baseline_ppl, by = "slno")



# DATA CHECKING!! ---------------------------------------------------------

# check to make sure summation works as expected. Returns "TRUE" if it works
sum(baseline_ppl$children_U5) == sum(x1$child_U5)
sum(baseline_ppl$children_5_17)== sum(x1$child_5_17)
sum(baseline_ppl$adult)== sum(x1$adult)
sum(baseline_ppl$all_ppl) == sum(table(x1$hhid))
sum(table(x1$hhid)) == sum(x1$child_U5) + sum(x1$child_5_17) + sum(x1$adult)


# CHECK to make sure there are not people who were not counted
check <- data.frame(1)
for (i in 1:nrow(x1)){
if(x1$child_U5[i] < 1 & x1$child_5_17[i] < 1 & x1$adult[i] <1) {
check[i,] <- 1
} else {
    check[i,]<-0  }
}

# Sums to 0 if everyone was counted
sum(check)



# SAVE DATA ---------------------------------------------------------------

save(baseline_ppl, file = "C:/Users/wrz741/Dropbox/C5_R_Codes/Rdata/baseline_ppl.Rdata")

