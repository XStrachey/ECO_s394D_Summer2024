library(tidyverse)
library(mosaic)

# Example 1: FedEx packages

fedex_sim = read.csv("data/raw data/fedex_sim.csv")

ggplot(fedex_sim) +
  geom_histogram(aes(x = weight), binwidth = 0.5)

favstats(~weight, data = fedex_sim)

do(5)*mean(~weight, data = sample_n(fedex_sim, 60))

# Monte Carlo simulation: 5000 samples of size n = 60
sim_n60 = do(5000)*mean(~weight, data = sample_n(fedex_sim, 60))

# histogram of the 5000 different means
ggplot(sim_n60) + 
  geom_histogram(aes(x = mean), binwidth = 0.1)

# Monte Carlo simulation: 5000 samples of size n = 300
sim_n300 = do(5000)*mean(~weight, data = sample_n(fedex_sim, 300))
