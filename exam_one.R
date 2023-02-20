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
run_expiriment <- function(n) {
  results <- replicate(n, system_functioning())
  return(mean(results))
}