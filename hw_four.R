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
  items <- rep(c(1,0), times=c(3,7))
  results <- NA
  for(i in 1:n) {
    draws <- sample(items, 5, F)
    results[i] <- sum(draws == 4)
  }
  return(mean(results))
}