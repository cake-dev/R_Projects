prob_at_least_1_2_given_sum_5 <- function(n) {
  y <- NA; z <- NA
  for(i in 1:n) {
    roll2 <- sample(x=1:6, size=2, replace=T)
    y[i] <- sum(roll2) # sums the 2 values from roll2
    z[i] <- any(roll2==2) # true if at least one value of 2 in roll2
    
    #table(y==5) #shows table where sum is 5
    #table(z[y==5]) # sub-setting z for only the sums that equal 5 ~ 1096 of them
    
  }
  return(mean(z[y==5])) # returns the proportion of values in z where the sum is 5
}

normplot <- function(m, sd) {
  # Set the mean and standard deviation
  mu <- m
  sigma <- sd
  
  # Create a sequence of x values
  x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 100)
  
  # Calculate the normal distribution density for each x value
  y <- dnorm(x, mean = mu, sd = sigma)
  
  # Plot the normal distribution
  plot(x, y, type = "l", xlab = "x", ylab = "Density", main=paste("Normal Dist with mean ",  mu, "and SD ",sigma, sep=" "))
  
  # Add vertical lines for mean and one standard deviation from mean
  abline(v = mu, lty = 2, col = "red", lwd = 2)
  abline(v = mu + sigma, lty = 2, col = "green", lwd = 2)
  abline(v = mu - sigma, lty = 2, col = "green", lwd = 2)
  
  # Add shading for the area between mean +/- one standard deviation
  polygon(x = c(mu - sigma, x[x >= mu - sigma & x <= mu + sigma], mu + sigma), 
          y = c(0, y[x >= mu - sigma & x <= mu + sigma], 0), 
          col = "#ADD8E6", border = NA)
  
  # Add legend for mean and standard deviation lines
  legend("topright", legend = c("Mean", "Mean +/- 1 SD"), 
         col = c("red", "#ADD8E6"), lty = c(2, 1), lwd = c(2, 10))
  
}


hw12 <- function(n) {
  s1 <- rnorm(1e6, 4.8, 1)
  s2 <- rnorm(1e6, 5.2, 1.5)
  ps1 <- 0.71
  ps2 <- 0.29
  results <- NA
  for(i in 1:n) {
    r1 <- runif(1)
    r2 <- runif(1)
  }
}

hw8q1 <- function(n) {
  set.seed(42)
  time_taken <- NA
  for(i in 1:n) {
    # get num users
    X <- rbinom(1, 500, 0.35)
    # sim num tasks for user
    Y <- rpois(X, 3.5)
    # sim time per task
    Z <- rgamma(sum(Y),10,4)
    time_taken[i] <- sum(Z)
  }
  q95 = quantile(time_taken, 0.95)/60
  minutes_add = (q95*60) - 1440
  df <- data.frame(
    Mean_Time=mean(time_taken)/60,
    Prob_24h_done=sum(time_taken<=24*60)/n,
    quan_95 = q95,
    Minutes_to_add=minutes_add
  )
  return(df)
}

hw8q3 <- function(n) {
  set.seed(42)
  time_taken <- NA
  for(i in 1:n) {
    # get num users
    X <- rbinom(1, 500, 0.35)
    # sim num tasks for user
    Y <- rpois(X, 3.5)
    # check if big job
    big_j <- sample(0:1, 1, prob=c(0.97, 0.03))
    if(big_j == 0) {
      # sim time per normal task
      Z <- rgamma(sum(Y),10,4)
    } else {
      # sim time per large task
      Z <- rgamma(sum(Y),3,0.2)
    }
    time_taken[i] <- sum(Z)
  }
  q95 = quantile(time_taken, 0.95)/60
  minutes_add = (q95*60) - 1440
  df <- data.frame(
    Mean_Time=mean(time_taken)/60,
    Prob_24h_done=sum(time_taken<=24*60)/n,
    quan_95 = q95,
    Minutes_to_add=minutes_add
  )
  
  return(df)
}