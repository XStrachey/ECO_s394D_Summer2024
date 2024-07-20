# load library
library(tidyverse)

# import data set
rapidcity = read.csv("data/raw data/rapidcity.csv", header = TRUE)

ggplot(rapidcity) + 
  geom_histogram(aes(x = Temp), binwidth = 2)

rapidcity %>%
  # The right-hand side of this statement, mean(Temp), tells R what summary statistic to calculate.
  # The left-hand side of the statement, avg_temp, is what name we want to give this summary.
  summarize(avg_temp = mean(Temp),
            median_temp = median(Temp),
            sd_temp = sd(Temp),
            # the inter-quartile range, or IQR—that is, from the 25th to the 75th percentiles.
            iqr_temp = IQR(Temp),
            min_temp = min(Temp),
            max_temp = max(Temp),
            q05_temp = quantile(Temp, 0.05),
            q95_temp = quantile(Temp, 0.95)) %>%
  round(1)

# In general, we think of |z|≈2 as the very lower limit of 
# where a data point might be considered “unusual” or “surprising” 
# relative to other numbers from the same distribution.
rapidcity = rapidcity %>%
  mutate(z = (Temp - mean(Temp)) / sd(Temp))