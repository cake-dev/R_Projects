system_functioning <- function() {
  p_fail <- 0.05
  # generate random values for each component to check against after one "year"
  r_X1 = runif(1)
  r_X2 = runif(1)
  r_X3 = runif(1)
  r_X4 = runif(1)
  # create the system
  c_system <- c(r_X1, r_X2, r_X3, r_X4)
  system_state <- c_system > p_fail
  return(sum(system_state)>2)
}
ex_one <- function(n) {
  results <- replicate(n, system_functioning())
  return(mean(results))
}

experiment_one <- function(n) {
  results <- NA
  system_state <- NA
  for(i in 1:n) {
    sys <- sample(c(0,1), size = 4, replace = TRUE, prob = c(0.05, 0.95))
    system_state <- sum(sys) >=3
    results[i] <- system_state
  }
  return(mean(results))
}

experiment_two <- function(n) {
  results <- NA
  databases <- c(0,0,0,1,0,1,0,1,0,1,0,1)
  for(i in 1:n) {
    searches <- sample(databases, 5, F)
    min_2_found <- sum(searches) >= 2
    results[i] <- min_2_found
  }
  return(mean(results))
}

experiment_three <- function(n) {
  # this works but it feels ugly
  all_parts <- NA
  for(i in 1:n) {
    if(runif(1) >= 0.6) { # if part made by B
      part <- sample(c("B","Bd"), 1, prob=c(0.95, 0.05))
    } else {
      part <- sample(c("A","Ad"), 1, prob=c(0.97, 0.03))
    }
    all_parts[i] <- part
  }
  prob <- (sum(all_parts=="Bd")) / (sum(all_parts=="Ad" | all_parts=="Bd"))
  return(prob)
}