win_on_fourth <- function(rolls) {
  A_W = c(1,2)
  B_W = c(1,2,3)
  C_W = c(1,2,3,4)
  if(any(rolls[1] %in% A_W)) {
    return(FALSE)
  }
  else if(any(rolls[2] %in% B_W)) {
    return(FALSE)
  }
  else if(any(rolls[3] %in% C_W)) {
    return(FALSE)
  }
  else if(any(rolls[4] %in% A_W)) {
    return(TRUE)
  } else {
    return((FALSE))
  }
}

experiment_2 <- function(n) {
  results <- NA
  for(i in 1:n) {
    rolls <- sample(x=1:6, size=4, replace=T)
    results[i] <- win_on_fourth(rolls)
  }
  return(mean(results))
}

experiment_3 <- function(n) {
  items <- rep(c("bad","good"), times=c(3,7))
  results_5 <- NA
  results_all <- NA
  for(i in 1:n) {
    for(j in 1:5) {
      draws <- sample(items, 2, F)
      results_5[j] <- sum(draws == "bad")
    }
    results_all[i] <- sum(results_5)
  }
  return(mean(results_all == 4))
}