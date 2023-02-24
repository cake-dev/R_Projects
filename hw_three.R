# this function draws 3 balls from an unbalanced bag
# it checks if all 3 match (unique length 1), then adds 1 or 0 depending
# it returns the ratio of same ball draws to all draws
set.seed(42)
draw_three_balls_all_same <- function(n) {
  results <- NA
  bag = rep(c('W', 'R', 'B'), times=c(6,5,4))
  for (i in 1:n) {
    the_draws <- sample(bag, size = 3, replace = FALSE)
    if (length(unique(the_draws)) == 1) {
      results[i] <- 1
    } else {
      results[i] <- 0
    }
  }
  return(mean(results))
}

arrow_expiriment <- function() {
  p_X <- 0.7;p_M <- 0.6;p_C <- 0.4
  # "shoot" the arrows by generating a number between 0 and 1
  r_X <- runif(1)
  r_M <- runif(1)
  r_C <- runif(1)
  # Check if exactly one archer hit the mark
  # these are the conditions that one archer hits and the other two miss
  if (r_X <= p_X & r_M > p_M & r_C > p_C |
      r_X > p_X & r_M <= p_M & r_C > p_C |
      r_X > p_X & r_M > p_M & r_C <= p_C) {
    
    # Check which archer hit the mark
    # this will return the given string to the replicate function
    if (r_X <= p_X) {
      "X"
    } else if (r_M <= p_M) {
      "M"
    } else {
      "C"
    }
  } else {
    "None"
  }
}

shoot_arrows_who_hit <- function(n) {
  # run the experiment n times (experiment says who hit when all three shoot with one arrow on the mark)
  results <- replicate(n, arrow_expiriment())
  # get the proportion of times each archer hit
  # note: we sum over the results only where 1 hit was achieved (!= none in this case)
  prop_X <- round(sum(results == "X") / sum(results != "None"), 4)
  prop_M <- round(sum(results == "M") / sum(results != "None"), 4)
  prop_C <- round(sum(results == "C") / sum(results != "None"), 4)
  
  df_hits = data.frame(
    Xandra = c(prop_X),
    Marco = c(prop_M),
    Cade = c(prop_C)
  )
  # return the formatted data
  return(df_hits)
}