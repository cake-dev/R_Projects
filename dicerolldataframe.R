# an R script to roll a dice 10000 times, then display the results in a dataframe sorted by value with the number of times the value was rolled

library(magrittr)
# library(tidyverse)

# create a vector of 1 through 6
die <- 1:6

# roll the die 10000 times
die_rolls <- sample(x = die, size = 10000, replace = TRUE)

# create a dataframe with the results
die_rolls_df <- data.frame(die_rolls)

# rename the column
names(die_rolls_df) <- "value"

# sort the dataframe by value
die_rolls_df <- die_rolls_df[order(die_rolls_df$value), ]

# count the number of times each value was rolled
die_rolls_df <- die_rolls_df %>%
    group_by(value) %>%
    summarise(count = n())

# display the results
die_rolls_df
