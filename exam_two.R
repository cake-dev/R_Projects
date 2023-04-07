# function to simulate one iteration of experiment
duck_catch_experiment <- function() {
  p_standard <- 0.7
  p_expert <- 0.1
  # random number between 0 and 1 for each duck
  random_nums <- runif(3)
  # catch the ducks (1 if caught, 0 if not)
  # fill all 3 values, then overwrite last one for expert duck
  ducks_caught <- ifelse(random_nums < p_standard, 1, 0)
  ducks_caught[3] <- ifelse(random_nums[3] < p_expert, 1, 0)
  num_ducks <- sum(ducks_caught)
  
  return(num_ducks)
}

duck_catching <- function(n) {
  # simulate the experiment n times
  results <- replicate(n, duck_catch_experiment())
  # calculate E(X) and Var(X)
  mean_x <- mean(results)
  var_x <- var(results)
  # print the results
  cat("E(X):", mean_x, "\n")
  cat("Var(X):", var_x)
}

duck_choose_experiment <- function() {
  ducks <- c(1,2,3,4)
  chosen_ducks <- sample(ducks, 2, replace = FALSE)
  return(min(chosen_ducks))
}

duck_choosing <- function(n) {
  results <- replicate(n, duck_choose_experiment())
  mean_x <- mean(results)
  var_x <- var(results)
  # print the results
  cat("E(X):", mean_x, "\n")
  table(results)/n
}