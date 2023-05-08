library(Sleuth2)

# 3
ex0223[ex0223$Increase == 'Yes' & ex0223$FatalitiesChange < 0, 3]

# 4A
X <- ex0223[ex0223$Increase == 'Yes',3]
Y <- ex0223[ex0223$Increase == 'No',3]

# 4B 
bs_1 <- sample(X, length(X), replace = T)

# 4C
bs_means_yes <- NA
for(i in 1:10000) {
  bs <- sample(X, length(X), replace = T)
  bs_means_yes[i] <- mean(bs)
}

bs_means_no <- NA
for(i in 1:10000) {
  bs <- sample(Y, length(Y), replace = T)
  bs_means_no[i] <- mean(bs)
}

# 4D
hist(bs_means_yes, main='Histogram of 10k Bootstrapped Means of ex0223', xlab='Mean % Change Fatalities', ylab='Frequency')


# 4E
e4 <- quantile(bs_means_yes, 0.975) - quantile(bs_means_yes, 0.025)

# 4F
f4 <- quantile(bs_means_no, 0.975) - quantile(bs_means_no, 0.025)

# 4G
g4 <- function(n) {
  bs_means_diff1 <- NA
  bs_means_diff2 <- NA
  diffs1 <- bs_means_no - bs_means_yes
  diffs2 <- bs_means_yes - bs_means_no
  for(i in 1:n) {
    bs_means_diff1[i] <- mean(diffs1[i])
    bs_means_diff2[i] <- mean(diffs2[i])
  }
  df <- data.frame(
    No_YesDiff=quantile(bs_means_diff1, 0.975) - quantile(bs_means_diff1, 0.025),
    Yes_Nodiff=quantile(bs_means_diff2, 0.975) - quantile(bs_means_diff2, 0.025)
  )
  return(df)
}

# 5F
f5 <- function(n) {
  results <- rbinom(n, 7000, 0.01)
  return(results)
}


# Exam 3 practice --------------------------------
# 2
lb_func <- function(x) {
  return((1/100)*(exp(1)^(-x/100)))
}

# 5
b4 <- function(n, s){
  results <- NA
  for(i in 1:n) {
    results[i] <- rbinom(1, s, 0.15)/s
  }
  return(results)
}