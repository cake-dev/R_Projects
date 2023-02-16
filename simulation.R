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
  