roll_4dice_sum <- function(n) {
  die <- 1:6
  sums <- NA
  for(i in 1:n) {
    rolls <- sample(x=die, size=4, replace=TRUE)
    sums[i] <- sum(rolls)
  }
  return(table(sums))
}