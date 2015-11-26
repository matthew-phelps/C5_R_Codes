# PHONE -------------------------------------------------------------------

n_phone <- 324.5
lambda_phone <- 37/n_phone


# Check if lambda * n is large - then we can use the normal approximation of pois
lambda_phone * n_phone 

# Method 1: Normal Approx
lambda_phone + 1.96 * sqrt(lambda_phone / n_phone) # Upper bound 
lambda_phone - 1.96 * sqrt(lambda_phone / n_phone) # Lower bound 

# Method 2:Simulation
x <- 0
for (i in 1:30000){
  x[i] <- sum(rpois(325, lambda_phone))
}

# Take middel 95% of simulations
x <- x[order(x)]
l <-.975 * length(x)
u <- 0.025 * length(x)
x_2 <- x[l:u]
lower <- min(x_2)
upper <- max(x_2)
lower/n_phone # Lower bound 
upper/n_phone # Upper bound 

# Method 3: Exact estimates
exactPoiCI <- function (X, conf.level=0.95) {
  alpha = 1 - conf.level
  upper <- 0.5 * qchisq(1-alpha/2, 2*X+2)
  lower <- 0.5 * qchisq(alpha/2, 2*X)
  return(c(lower, upper))
}


est_phone_upper <- exactPoiCI(4)[2]
est_phone_lower <- exactPoiCI(4)[1]

est_phone_upper / 14.4 # Upper bound 
est_phone_lower / 14.4 # Lower bound 


