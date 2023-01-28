draw_first_letter <- function(n) {
  name <- c("J","A","K","E")
  draws <- sample(x=name, size=1000, replace=TRUE)
  return(mean(draws=="J"))
}

roll_3dice_sum14 <- function(n) {
  die <- 1:6
  sums <- c()
  for(i in 1:n) {
    rolls <- sample(x=die, size=3, replace=TRUE)
    sums <- append(sums, sum(rolls))
  }
  return(mean(sums==14))
}

set.seed(12)
inclass <- function() {
  return(sample(x=c(40,0,-10), size=100, replace=TRUE, prob=c(1/6, 5/36, 25/36)))
}

play_n_times <- function(n) {
  results <- NA
  for(i in 1:n) {
    results[i] <- sum(inclass())
  }
  return(results)
}