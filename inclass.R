# # 1 vector of nums 3, 12, and 8
# myVector <- c(3,12,8)
# 
# # 2 use the sum() function to find the total of the values in myVector
# sum(myVector)
# 
# # 3 use the == operator to check if each value in myVector is equal to 12
# print(myVector == 12) # checks full vector and returns list of true false
# 
# for(i in 1:length(myVector)) {
#   if (myVector[i] == 12) {
#     print(sprintf("12 at position %s", i))
#   }
# }
# 
# # 4 simulate sampling from myVector 100 times. use the set.seed() to set the see for psuedorandom sampling to 12. save
# # the 100 values to mySamples
# set.seed(12)
# mySamples <- sample(myVector, 100, TRUE)
# 
# # 5 find the sum of mySamples, should be 798 w/ seed 12
# sum(mySamples)
# 
# # 6 loop to print hello 5 times
# for (i in 1:5) {
#   print("hello")
# }
# 
# # 7 loop tp print integers 1 to 10
# for(i in 1:10) {
#   print(i)
# }
# 
# # 8 obtain first 5 values of mySamples
# mySamples[1:5]
# 
# # 9 Print ith value of mySamples
# for (i in 1:length(mySamples)) {
#   #print(mySamples[i])
# }
# 
# # 10 sum first i value of mySamples
# sum = 0
# for (i in 1:length(mySamples)) {
#   sum = sum + mySamples[i]
# }
# sum

# simulate P(2 heads total in 5 in flips)
heads_in_flips <- function (n){
  results <- NA
  for (i in 1:n) {
    flips <- sample(c("H", "T"), 5, replace=TRUE)
    # bool_flips <- flips=="H"
    count <- sum(bool_flips)
    results[i] <- count==2
    #mean(bool_flips==T)
    #results[i] <- flips
  }
  return(mean(results)) # only observes true values bc they are 1, false is 0
}

roll2diceconditional <- function(n) {
  die <- 1:6
  totals_min_seven <- NA
  rolls_4 <- NA
  for(i in 1:n) {
    rolls <- sample(die, 2, T)
    # was there exactly one 4?
    rolls_4[i] <- sum(rolls == 4) == 1
    total <- sum(rolls)
    # was the total at least 7?
    totals_min_seven[i] <- sum(rolls) >= 7
  }
  return(mean(totals_min_seven[rolls_4]))
}

roll2dice_absvaldiff <- function(n) {
  # returns a PMF of the probabilities of differences between 2 rolls
  die <- 1:6
  diffs <- NA
  for(i in 1:n) {
    roll <- sample(die, 2, T)
    diffs[i] <- abs(roll[1] - roll[2])
  }
  return(table(diffs)/n)
}

game_winnings_expected <- function(n) {
  d <- 1:6
  profit <- c(2,3,5)
  results <- NA
  for(i in 1:n) {
    roll <- sample(d, 1)
    if(roll %in% profit) {
      results[i] <- roll
    } else {
      results[i] <- 0
    }
  }
  return(mean(results))
}

game_winnings_prob <- function(n) {
  winnings <- NA
  for(i in 1:n) {
    rolls <- sample(1:6, 1)
    winnings[i] <- 2*(rolls==2) + 3*(rolls==3) + 5*(rolls==5)
  }
  return((winnings))
}

plot_cum_mean <- function(values) {
  cumAvg <- cumsum(values)/(1:length(values))
  plot(x = 1:length(values), # data points
       y = cumAvg, # data points
       type = "l", # line graph
       log='x', # set log scale on x axis
       xlab="Number of trials", # x axis label
       ylab="Cumulative mean",  # y axis label
       lwd=1.2, # line width
       cex.lab=1.3 # makes labels larger
       )
  abline(h=5/3, lty=2)
}