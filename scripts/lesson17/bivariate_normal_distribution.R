library(tidyverse)
library(extraDistr)

# Application: stocks and bonds

# Version 1: negative correlation.

# parameters for bivariate normal
mu_stocks = 0.06
mu_bonds = 0.03
sigma_stocks = 0.2
sigma_bonds = 0.1
rho = -0.5  # correlation

# a single draw of stock/bond returns from the bivariate normal model
rbvnorm(n = 1, mu_stocks, mu_bonds, sigma_stocks, sigma_bonds, rho)

# Set-up
horizon = 40   # length of our investing horizon in years
weights = c(0.5, 0.5) # fraction of wealth in each asset?

portfolio_sb_1 = do(10000)*{
  total_wealth = 10000  # reset our initial wealth for each pass
  wealth_by_asset = total_wealth * weights
  for(t in 1:horizon) {
    # 1) Simulate returns
    returns = rbvnorm(1, mu_stocks, mu_bonds, sigma_stocks, sigma_bonds, rho)
    # 2) Recursively update wealth
    wealth_by_asset = wealth_by_asset * (1 + returns)
    # 3) Rebalance
    total_wealth = sum(wealth_by_asset)
    wealth_by_asset = total_wealth * weights
  }
  c(rho = rho, result = total_wealth)  # save the final value of total wealth from each pass
}

ggplot(portfolio_sb_1) + 
  geom_histogram(aes(x = result), bins = 50)

# along with some summary statistics from favstats
favstats(~result, data = portfolio_sb_1)

# Version 2: positive correlation.

# parameters for bivariate normal
mu_stocks = 0.06
mu_bonds = 0.03
sigma_stocks = 0.2
sigma_bonds = 0.1
rho = 0.5  # now positive correlation

# Set-up
horizon = 40   # length of our investing horizon in years
weights = c(0.5, 0.5) # fraction of wealth in each asset?

portfolio_sb_2 = do(10000)*{
  total_wealth = 10000  # reset our initial wealth for each pass
  wealth_by_asset = total_wealth * weights
  for(t in 1:horizon) {
    # 1) Simulate returns
    returns = rbvnorm(1, mu_stocks, mu_bonds, sigma_stocks, sigma_bonds, rho)
    # 2) Recursively update wealth
    wealth_by_asset = wealth_by_asset * (1 + returns)
    # 3) Rebalance
    total_wealth = sum(wealth_by_asset)
    wealth_by_asset = total_wealth * weights
  }
  c(rho = rho, result = total_wealth)  # save the final value of total wealth from each pass
}

portfolios_all = bind_rows(portfolio_sb_1, portfolio_sb_2)

portfolios_all %>%
  group_by(rho) %>%
  summarize(fav_stats(result),
            p_lose_money = sum(result < 10000) / n())

ggplot(portfolios_all) + 
  geom_histogram(aes(x=result)) + 
  facet_wrap(~rho, nrow=2) + 
  scale_x_log10()
# The lesson here is that negative correlation equals diversification. 
# When one asset performs poorly, the other tends to perform well, on average, 
# thereby picking up the slack in your portfolio and leading to a reduced level of risk overall.