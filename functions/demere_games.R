
# die object
die <- 1:6

# roll a die
sample(die, size = 1)
sample(die, size = 1)
sample(die, size = 1)
sample(die, size = 1)

# function
roll <- function() {
  die <- 1:6
  sample(die, size = 1)
}

roll()


# roll it 10 times
for (i in 1:10) {
  roll()
}


# roll it 10 times
for (i in 1:10) {
  print(roll())
}

# keep rolling until first 3
repeat {
  rol <- roll()
  print(rol)
  if (rol == 3) break
}

# keep rolling until first 3
rol <- 1

while (rol != 3) {
  rol <- roll()
  print(rol)
}


# rolling a pair of dice
roll()
roll()

# sum of points
roll() + roll()
roll() + roll()
roll() + roll()


sample(die, size = 2)

sample(die, size = 2, replace = TRUE)


roll <- function() {
  die <- 1:6
  sum(sample(die, size = 2, replace = TRUE))
}
roll()


# De Mere
# roll two dice, win if at least one 6
times <- 500
rolls <- 4
results <- matrix(0, nrow = times, ncol = rolls)
for (i in 1:times) {
  results[i, ] <- sample(die, size = rolls, replace = TRUE)
}

head(results)

counts <- 0
for (i in 1:times) {
  if (any(results[i, ] == 6))
    counts <- counts + 1
}
counts
counts / times

