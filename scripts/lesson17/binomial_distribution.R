library(tidyverse)

# The binomial distribution

# The way we calculate these binomial probabilities in R is using the dbinom function.

# We’d expect to see exactly 7 no-shows with the probability 0.09
# out of total 140 peoples under binomial probability distribution
dbinom(7, size = 140, prob = 0.09)

dbinom(13, size = 140, prob = 0.09)

# First we create a tibble, or table of data, that has a single column called k, 
# containing the numbers 0 through 30 (0:30).
airlines = tibble(k = 0:30)
airlines <- airlines %>%
  mutate(prob = dbinom(k, 140, 0.09))

ggplot(airlines) +
  geom_col(aes(x = k, y = prob))

# The function called pbinom calculates the sum of probabilities lower equal x.
# That means pbinom is a function work as the cumulative distribution function.
pbinom(5, 140, 0.09)
# So according to the model, there’s about a 1.1% chance of 5 or fewer no-shows.
# [1] 0.01098229