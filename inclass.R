# 1 vector of nums 3, 12, and 8
myVector <- c(3,12,8)

# 2 use the sum() function to find the total of the values in myVector
sum(myVector)

# 3 use the == operator to check if each value in myVector is equal to 12
print(myVector == 12) # checks full vector and returns list of true false

for(i in 1:length(myVector)) {
  if (myVector[i] == 12) {
    print(sprintf("12 at position %s", i))
  }
}

# 4 simulate sampling from myVector 100 times. use the set.seed() to set the see for psuedorandom sampling to 12. save
# the 100 values to mySamples
set.seed(12)
mySamples <- sample(myVector, 100, TRUE)

# 5 find the sum of mySamples, should be 798 w/ seed 12
sum(mySamples)

# 6 loop to print hello 5 times
for (i in 1:5) {
  print("hello")
}

# 7 loop tp print integers 1 to 10
for(i in 1:10) {
  print(i)
}

# 8 obtain first 5 values of mySamples
mySamples[1:5]

# 9 Print ith value of mySamples
for (i in 1:length(mySamples)) {
  #print(mySamples[i])
}

# 10 sum first i value of mySamples
sum = 0
for (i in 1:length(mySamples)) {
  sum = sum + mySamples[i]
}
sum