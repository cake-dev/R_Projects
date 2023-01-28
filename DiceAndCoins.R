roll_n <- function(n) {
  die <- 1:6
  return(sample(die, size=n, replace=TRUE))
}

roll2 <- function() {
  die <- 1:6
  d1 <- sample(die, size = 1, replace = TRUE)
  d2 <- sample(die, size = 1, replace = TRUE)
  return(c(d1,d2))
}

flip2 <- function() {
  coin <- 1:2
  f1 <- sample(coin, size = 1)
  f2 <- sample(coin, size = 1)
  return(c(f1,f2))
}

flip3 <- function() {
  coin <- 1:2
  f1 <- sample(coin, size = 1)
  f2 <- sample(coin, size = 1)
  f3 <- sample(coin, size = 1)
  return(c(f1,f2,f3))
}

flip_two_get_one_head <- function(n) {
  all_flips <- c()
  for (i in 1:n) {
    flips <- flip2()
    f1 = flips[1]
    f2 = flips[2]
    all_flips <- c(all_flips, f1,f2)
  }
  return(all_flips)
}

flip_three_get_one_head <- function(n) {
  all_flips <- c()
  for (i in 1:n) {
    flips <- flip3()
    f1 = flips[1]
    f2 = flips[2]
    f3 = flips[3]
    all_flips <- c(all_flips, c(f1,f2,f3))
  }
  return(all_flips)
}

roll_second_lessthan_first <- function(n) {
  sum = 0
  for (i in 1:n) {
    rolls <- roll()
    r1 <- rolls[1]
    r2 <- rolls[2]
    if (r2 < r1) {
      sum = sum + 1
    }
  }
  return (sum/n)
}
