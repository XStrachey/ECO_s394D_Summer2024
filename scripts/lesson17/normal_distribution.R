library(tidyverse)
library(mosaic)

aapl_returns = read.csv("data/raw data/aapl_returns.csv", header = TRUE)

ggplot(aapl_returns) +
  geom_histogram(aes(x = return, y = ..density..), binwidth = 0.005)

favstats(~return, data = aapl_returns) %>% round(4)

# count the really bad days where aapl lost at least 8% of its value in a single day
aapl_returns %>%
  count(return <= -0.08)

aapl_returns %>%
  count(return >= 0.10)

# function pnorm can give us how likely is such a four-sigma loss
# (lost at least 8% of its value in a single day) under the normal model
pnorm(-0.08, mean = 0.0013, sd = 0.0202)
# specifying that we want an upper-tail rather than a lower-tail probability
pnorm(0.10, mean = 0.0013, sd = 0.0202, lower.tail = FALSE)

annual_returns_since1928 = read.csv("data/raw data/annual_returns_since1928.csv", header = TRUE)

annual_returns_since1928 = annual_returns_since1928 %>%
  # SP500_net, that measures each year’s “whole-market” return, net of inflation
  mutate(SP500_net = SP500 - Inflation)

# compute the mean and standard deviation
favstats(~SP500_net, data = annual_returns_since1928) %>% 
  round(4)

ggplot(annual_returns_since1928) +
  geom_histogram(aes(x = SP500_net, y = ..density..), binwidth = 0.05)

# run a Monte Carlo simulation.
horizon=40
portfolio_stocks = do(10000)*{  # beginning of outer loop
  current_wealth = 10000 # reset at the beginning of each simulated trajectory
  
  for(t in 1:horizon) {  # start of inner loop
    return_t = rnorm(1, mean = 0.06, sd = 0.2)   # generate random return
    current_wealth = current_wealth * (1 + return_t)   # update wealth via simple interest formula
  }  # end of inner loop
  
  # save the final value of wealth
  current_wealth
}  # end of outer loop
# Upon inspecting portfolio_stocks, we find that it has a single column called result

ggplot(portfolio_stocks) + 
  geom_histogram(aes(x = result), bins=50)

favstats(~result, data = portfolio_stocks) %>% round(0)

# And based on our simulation, what’s the chance that you’ll lose money?
portfolio_stocks %>% count(result < 10000)