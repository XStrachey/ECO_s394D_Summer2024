library(tidyverse)
library(mosaic)

# Example 1: dessert

# This function is designed to simulate coin flips
rflip(100, prob = 0.52)

# Remember the basic mantra of statistical inference: you’re certain if your results are repeatable.
# Let’s repeat our hypothetical survey 10 times
dessert_surveys <- do(10000)*nflip(100, prob = 0.52)

dessert_surveys = dessert_surveys %>%
  # Let’s divide these numbers by 100 (the notional sample size of each survey),
  # so we can interpret the results as a proportion
  mutate(huckleberry_prop = nflip / 100)

# This histogram depicts the central object of concern in statistical inference. 
# It is an example of what’s called a sampling distribution.
ggplot(dessert_surveys) + 
  geom_histogram(aes(x = huckleberry_prop), binwidth = 0.01)
# If your data is the result of a random process, 
# then any statistical summary you compute from that data will have a sampling distribution.

# The standard deviation of a sampling distribution is called the standard error. 
# This reflects the typical statistical fluctuation of our summary statistic
dessert_surveys %>%
  summarize(std_err = sd(huckleberry_prop))
# By adding a “plus or minus” based on the standard error, 
# we’re telling our audience not to expect any more precision than 
# what the data are capable of offering.

