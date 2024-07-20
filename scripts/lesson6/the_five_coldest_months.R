# Our main high-level task is to find the five coldest individual months in our data set on Rapid City, 
# which spans the seventeen years from 1995 to 2011.

library(tidyverse)

rapidcity = read.csv("data/raw data/rapidcity.csv", header = TRUE)

rapidcity %>%
  # When we call group_by with multiple variables, 
  # it groups the data according to all possible combinations of those variables.
  group_by(Year, Month) %>%
  summarize(avg_temp = mean(Temp),
            coldest_day = min(Temp),
            warmest_day = max(Temp)) %>%
  arrange(avg_temp) %>%
  head(5) %>%
  round(1)