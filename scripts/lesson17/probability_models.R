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

# The Poisson distribution

# To calculate probabilities under the poisson distribution, we can use the dpois function in R.

man_city_probs = tibble(k = 0:8)
man_city_probs <- man_city_prob %>%
  mutate(prob = dpois(k, lambda = 2.18))

ggplot(man_city_probs) +
  geom_col(aes(x = k, y = prob))

arsenal_probs = tibble(k = 0:8)
arsenal_probs <- arsenal_probs %>%
  mutate(prob = dpois(k, lambda = 1.45))

ggplot(arsenal_probs) +
  geom_col(aes(x = k, y = prob))

# if we wanted to estimate the probability of a 2-1 win for Man City, 
# we can multiply the two Poisson probabilities together under the assumption of independence.
dpois(2, lambda = 2.18) * dpois(1, lambda = 1.45)
# [1] 0.09136125

soccer_scores = tibble(man_city = 0:6, arsenal = 0:6)

soccer_probs = soccer_scores %>%
  tidyr::expand(man_city, arsenal) %>%
  mutate(prob = dpois(man_city, 2.18) * dpois(arsenal, 1.45))

ggplot(soccer_probs) +
  # Make a heat map, which will array the possible scores in a 2D grid of tiles.
  geom_tile(aes(x = man_city, y = arsenal, fill = prob)) +
  geom_text(aes(x = man_city, y = arsenal, label = round(prob, 3)), color='darkblue') + 
  scale_fill_gradient(low = "white", high = "red")

# calculate the overall probability of a Man City win
soccer_probs %>% 
  filter(man_city > arsenal) %>%
  summarize(sum(prob))

# calculate the draw
soccer_probs %>% 
  filter(man_city == arsenal) %>%
  summarize(sum(prob))

# calculate Arsenal win
soccer_probs %>%
  filter(man_city < arsenal) %>%
  summarize(sum(prob))

# use the ppois function, which works much like pbinom did above
ppois(6, lambda = 2.18, lower.tail = FALSE)
# [1] 0.007118364

# use the rpois function to run a Monte Carlo simulation
NMC = 100000   # number of Monte Carlo simulations
arsenal = rpois(NMC, 1.45)
ManCity = rpois(NMC, 2.18)

# here’s the approximate probability of an Arsenal win in simulated game outcomes
sum(arsenal > ManCity)/NMC

# Cross-tabulate the simulated results
xtabs(~arsenal + ManCity)