#' ---
#' title: "Confidence Intervals"
#' author: "Matthew Phelps"
#' date: "Jan 18, 2016"
#' ---
#
# PHONE data-------------------------------------------------------------------
#
n_phone <- 869.9 # person-time years at risk total in study
n_cases <- 97
lambda_phone <- n_cases/n_phone
lambda_phone # Point estimate

# If lambda * n is large - then we can use the normal approximation of pois

# Method 1: Normal Approx
lambda_phone + 1.96 * sqrt(lambda_phone / n_phone) # Upper bound 
lambda_phone - 1.96 * sqrt(lambda_phone / n_phone) # Lower bound 

# Method 2:Simulation
x <- 0
for (i in 1:30000){
  x[i] <- sum(rpois(n_phone, lambda_phone))
}

summary(x)
# Take middel 95% of simulations
x <- x[order(x)]
l <-.975 * length(x)
u <- 0.025 * length(x)
x_2 <- x[l:u]
upper <- max(x_2)
lower <- min(x_2)

upper/n_phone # Upper bound 
lower/n_phone # Lower bound 


# Method 3: Exact estimates
# NOT SURE ABOUTH THIS METHOD. Found at: http://tinyurl.com/hkrncpp
exactPoiCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  return(c(lower, upper))
}


est_phone_upper <- exactPoiCI(n_cases)[2]
est_phone_lower <- exactPoiCI(n_cases)[1]

est_phone_upper / n_phone # Upper bound 
est_phone_lower / n_phone # Lower bound 




#
# 48Hr --------------------------------------------------------------------
#

n_48hr <- 34.3
n_cases_48h <- 11
lambda_48hr <- n_cases_48h/n_48hr
lambda_48hr # Point estimate

# Check if lambda * n is large - then we can use the normal approximation of pois
lambda_48hr * n_48hr 

# Method 1: Normal Approx
lambda_48hr + 1.96 * sqrt(lambda_48hr / n_48hr)
lambda_48hr - 1.96 * sqrt(lambda_48hr / n_48hr)

# Method 2:Simulation
x <- 0
for (i in 1:30000){
  x[i] <- sum(rpois(n_48hr, lambda_48hr))
}
summary(x)

# Take middel 95% of simulations
x <- x[order(x)]
l <-.975 * length(x)
u <- 0.025 * length(x)
x_2 <- x[l:u]
upper <- max(x_2)
lower <- min(x_2)

upper/n_48hr
lower/n_48hr


# Method 3: Exact estimates
# NOT SURE ABOUTH THIS METHOD. Found at: http://tinyurl.com/hkrncpp
exactPoiCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  return(c(lower, upper))
}


est_48hr_upper <- exactPoiCI(n_cases_48h)[2]
est_48hr_lower <- exactPoiCI(n_cases_48h)[1]

est_48hr_upper / n_48hr
est_48hr_lower / n_48hr
