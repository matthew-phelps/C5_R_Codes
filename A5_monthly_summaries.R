# Author: Matthew Phelps
# Desc:    Monthly summaries for Leela. Record for each month:
#   * how many actiev HHs
#   * how many new phones distributed
#   * number of active participants
#   * number of monthly visits undertaken
# output: summary data
# DEPENDENCIES: Requires M1-M5 & B1, B2 to have been run


# Intro -------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)

#detach("package:plyr", unload=TRUE) # disrupts the dplyr package

# Prepare workspace: if user == CHAR prepare Char's path, else: MAtthew's path
rm(list = ls())

ifelse(grepl("zrc340", getwd()),
       baseline.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata",
       baseline.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\baseline_x1_merge.Rdata")
ifelse(grepl("zrc340", getwd()),
       m5.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\person-time.Rdata",
       m5.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\person-time.Rdata")
ifelse(grepl("zrc340", getwd()),
       functions.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\c_5_functions_source_file.R",
       functions.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\c_5_functions_source_file.R")
ifelse(grepl("zrc340", getwd()),
       output.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\monthly_summary.csv",
       output.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly_summary.csv")


ifelse(grepl("zrc340", getwd()),
       plot1.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\plot1.pdf",
       plot1.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\plot1.pdf")
ifelse(grepl("zrc340", getwd()),
       plot2.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\plot2.pdf",
       plot2.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\plot2.pdf")
ifelse(grepl("zrc340", getwd()),
       plot3.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\plot3.pdf",
       plot3.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\plot3.pdf")
ifelse(grepl("zrc340", getwd()),
       plot4.path <- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\plot4.pdf",
       plot4.path <-"C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\plot4.pdf")

# LOAD DATA ---------------------------------------------------------------

load(baseline.path)
load(m5.path)
source(functions.path)


# DATA PREPERATION -----------------------------------------------------

# Round dates to 1st of month for easier calculations
base_merge$phoneMonthYear <- floor_date(base_merge$phone.dist, unit = "month")
base_merge$withMonthYear <- floor_date(base_merge$with_date, unit = "month")
m5$date_visit_month <- floor_date(m5$date_visit, unit = "month")

# Remove un-used variables:
uniqueHH <- dplyr::select(base_merge, uniqueID, HH_key, HHID, base_date.x, with_date, phone.dist, phone.dist.original, phoneMonthYear, withMonthYear)
rm(base_merge)

# Check for duplicates - if duplicates founds, check problem upstream
x <- duplicated(uniqueHH[, 1])
sum(x)


# df of unique HH_keys. A HH that moves should only appear once in this df
unique_hh_key <- split(uniqueHH, f = uniqueHH$HH_key)

mergeHH_key <- function(x){
  if (nrow(x) > 1){
    # Take the min phone dist. and the max withdraw date of the HHs through
    # their moves
    x$base_date.x[1] <- min(x$base_date.x)
    x$with_date[1] <- max(x$with_date)
    x$phone.dist[1] <- min(x$phone.dist)
    x$phoneMonthYear[1] <- min(x$phoneMonthYear)
    x$withMonthYear[1] <- max(x$withMonthYear)
  }
  # Return only first record
  x <- x[1,]
  
  # uniqueID no longer makes sense, since many IDs merged to 1 HH_key
  x <- dplyr::select(x, -uniqueID)
  x
}

unique_hh_key <- lapply(unique_hh_key, mergeHH_key)
unique_hh_key <- do.call(rbind.data.frame, unique_hh_key)
row.names(unique_hh_key) <- NULL



# NEW PHONES PER MONTH ----------------------------------------------------------
# Counting how many phones were distributed each month

monthly_summary <- as.data.frame(table(unique_hh_key$phoneMonthYear))
monthly_summary$Var1 <- as.Date(monthly_summary$Var1)
monthly_summary <- dplyr::rename(monthly_summary, new_phones = Freq)


# ACTIVE HH & DROPOUT HH ------------------------------------------------

# Interval for households, ignoring moves. If HH moves, it remains active until 
# formal withdraw
unique_hh_key$active_interval <- new_interval(unique_hh_key$phoneMonthYear, unique_hh_key$withMonthYear)

month.names <- strftime(monthly_summary$Var1, format = "%b-%Y")
z <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(unique_hh_key)))
names(z) <- month.names

# Check if each month, during range of study period, is within the "active interval"
# for each household.
for (i in 1:nrow(monthly_summary)){
  for (j in 1:nrow(unique_hh_key)){
    z[j, i] <- monthly_summary$Var1[i] %within% unique_hh_key$active_interval [j]
    # Number of HHs dropping out or moving 
  }
}

# Number of individuals dropping out from HHs, but HHs remain in study
temp <- m5 %>%
  group_by(date_visit_month) %>%
  summarise(dropout_individuals = sum(old_per_out, na.rm = T))

# Merge into summary data.frame
z1 <- colSums(z)
monthly_summary$active_hh <- z1
monthly_summary <- left_join(monthly_summary, temp, by = c("Var1" = "date_visit_month"))

rm(z, zy, temp, z1, zy1, x, m5.path, i, j, baseline.path)




# DROPOUTS ----------------------------------------------------------------

dropout <- as.data.frame(table(unique_hh_key$withMonthYear))
dropout$Var1 <- as.Date(dropout$Var1)
dropout <- dplyr::rename(dropout, dropout_HH = Freq)
monthly_summary <- left_join(monthly_summary, dropout, by = "Var1")
sum(dropout$dropout_HH[1:(nrow(dropout) - 1)])

# ACTIVE PPL --------------------------------------------------------------


month_visit_ls <- split(m5, f = m5$HH_key)
t.temp <- t[[2]]
activePeople <- function(t.temp) {
  
  # Create time interval b/w visits
  g <- NA
  x <- new_interval(t.temp$date_visit_month[1], t.temp$date_visit_month[1]) 
  # If there is >1 monthly visit:
  if (nrow(t.temp)> 1){
    for (i in 1:nrow(t.temp)-1){
      g[i] <- t.temp$date_visit_month[i+1] - t.temp$date_visit_month[i]
      x[i] <- as.interval(g[i], t.temp$date_visit_month[i])
    }
  x[nrow(t.temp),] <- as.interval(t.temp$with_date[nrow(t.temp)] - t.temp$date_visit_month[nrow(t.temp)-1], t.temp$date_visit_month[nrow(t.temp)-1])
  
  # first interval - measure from phone.dist date to date of second monthly visit
  x[1,] <- as.interval(t.temp$date_visit_month[2] - floor_date(t.temp$phone.dist[1], unit = "month"), floor_date(t.temp$phone.dist[1], unit = "month"))
  } else { # if there is only 1 monthly visit, use interval from phone.dist to withdraw
    x <- as.interval((t.temp$with_date - t.temp$phone.dist), t.temp$phone.dist)
  }
  # associate each interval with the number of active ppl during that interval
  x <- as.data.frame(x)
  x <- rename(x, interval = x)
  t.temp <- cbind(t.temp, x)
  rm(x)
  
  # Figure out how many people are present during each time interval. Cuz there
  # are overlapping intervals, take the Max no. of ppl (no rational, just easier
  # than taking the min)
  h <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = nrow(t.temp)))
  h1 <- data.frame(matrix(ncol=nrow(monthly_summary), nrow = 1))
  names(h1) <- month.names
  for (i in 1:nrow(monthly_summary)){
    for (j in 1:nrow(t.temp)){
      h[j, i] <- (monthly_summary$Var1[i] %within% t.temp$interval[j]) * t.temp$ppl_all[j]
    }
    h1[i] <- max(h[,i])
  }
  return(h1)
}

vf <- lapply(month_visit_ls, activePeople)
cv <- do.call(rbind.data.frame, vf)
month.temp <- colSums(cv)
monthly_summary <- cbind(monthly_summary, month.temp)
monthly_summary <- rename(monthly_summary, active_ppl = month.temp)
rm(vf, cv, month.temp)


# NUMBER OF MONTHLY VISITS ------------------------------------------------

x <- as.data.frame(table(m5$date_visit_month))
x$Var1 <- as.Date(x$Var1)
monthly_summary <- left_join(monthly_summary, x, by = "Var1")
monthly_summary <- rename(monthly_summary, number_visits = Freq, date = Var1)



# PLOTS -------------------------------------------------------------------
graphics.off()
plot1 <- ggplot(data = monthly_summary, aes(x = date, y = new_phones)) +
  geom_bar(stat = "identity") +
  ggtitle("New phones") +
  theme(plot.title = element_text(size = 20, face="bold"))
plot1

plot2 <-ggplot(data = monthly_summary, aes(x = date, y = active_hh)) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.8) +
  ggtitle("Active households") +
  theme(plot.title = element_text(size = 20, face="bold"))
plot2

plot3 <- ggplot(data = monthly_summary, aes(x = date, y = number_visits)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.8) +
  ggtitle("Number of monthly visits")+
  theme(plot.title = element_text(size = 20, face="bold"))
plot3

plot4 <- ggplot(data = monthly_summary, aes(x = date, y = dropout_individuals/active_ppl)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  ggtitle("Individuals dropping out from active households\n as % of active paricipants")+
  theme(plot.title = element_text(size = 20, face="bold"))
plot4

plot5 <- ggplot(data = monthly_summary, aes( x= date, y = dropout_HH/active_hh)) +
  geom_bar (stat = "identity", fill = "purple", alpha = 0.8) +
  ggtitle ("Household dropouts as % of active households") +
  theme(plot.title = element_text(size = 20, face = "bold"))
plot5
# WRITE OUTPUT ------------------------------------------------------------

write.csv(monthly_summary, file = output.path)

ggsave(filename = plot1.path, plot = plot1, width = 10, height = 10)
ggsave(filename = plot2.path, plot = plot2, width = 10, height = 10)
ggsave(filename = plot3.path, plot = plot3, width = 10, height = 10)
ggsave(filename = plot4.path, plot = plot4, width = 10, height = 10)

