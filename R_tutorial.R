# Vector (a 1D array of objects)
my_vector <- 1:6 # creates a vector with numbers 1 through 6, and assigns it to my_vector
my_vector > 3 # returns a vector with TRUE / FALSE values based on the condition

# Concatenate function -> returns a vector of the provided objects
vector_empty <- c() # empty vector
vector_ints <- c(1, 2, 3) # a vector of 3 integer values
vector_bools <- c(FALSE, TRUE, FALSE) # a vector of 3 Boolean values
vector_strings <- c("dog", "cat", "bird") # a vector of 3 string values

# Find the mean
mean(c(1, 2, 3, 4, 5)) # returns the average of the provided collection of values
# 
# randomly sample from a list of things
sample(x = my_vector, size = 3) # returns 3 items sampled from the provided collection
sample(x = 1:10, size = 100, replace = TRUE) # returns 100 things from the collection 1 through 10, while replacing values already chosen.  this allows us to pick multiple from the same collection

# roll a 6 sided die 100000 times
die_rolls <- sample(x = 1:6, size = 100000, replace = TRUE)
# test to see which rolls are equal to 4
# die_rolls == 4
table(die_rolls == 4)

# our simulated probability of rolling is the # of 4's observed

# in R, TRUE == 1, FALSE == 0
mean(die_rolls == 4) # tells us what proportion of the values equal 4

# can set the seed (starting point) for the randomness
set.seed(123)
