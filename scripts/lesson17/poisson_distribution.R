library(tidyverse)
# The Poisson distribution

# To calculate probabilities under the poisson distribution, we can use the dpois function in R.

man_city_probs = tibble(k = 0:8)
man_city_probs <- man_city_probs %>%
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