# load library
library(tidyverse)

ggplot(rapidcity) + 
  geom_boxplot(aes(x = factor(Month), y = Temp))

# group_by

rapidcity %>%
  # group the observations by the Month variable.
  group_by(Month) %>%
  # calculate summary statistics separately for each month.
  summarize(avg_temp = mean(Temp),
            sd_temp = sd(Temp),
            q05_temp = quantile(Temp, 0.05),
            q95_temp = quantile(Temp, 0.95)) %>%
  round(1)

rapidcity_summary = rapidcity %>%
  group_by(Month) %>%
  summarize(avg_temp = mean(Temp),
            # get current group size.
            prop_freeze = sum(Temp <= 32) / n())

ggplot(rapidcity_summary) + 
  # The command factor overrides this default behavior, 
  # telling R to treat the number as a label, 
  # not as a number with a meaningful magnitude.
  geom_col(aes(x = factor(Month), y = avg_temp))

ggplot(rapidcity_summary) + 
  geom_col(aes(x = factor(Month), y = prop_freeze))

# filter

# create a 2009-only subset
rapidcity2009 = rapidcity %>%
  filter(Year == 2009)

head(rapidcity2009)

rapidcity %>%
  # a subset of the data that spans 2006–09
  filter(Year >= 2006 & Year <= 2009) %>%
  # group by month
  group_by(Month) %>%
  # calculate summary statistics
  summarize(avg_temp = mean(Temp),
            sd_temp = sd(Temp)) %>%
  round(1)

# select

# We can use select to pick only the variables we want to retain.
rapidcity2009 %>%
  select(Month, Day, Temp) %>%
  head()

# We could have accomplished the same task by saying select(-Year).
rapidcity2009 %>%
  select(-Year) %>%
  head

# mutate

rapidcity_augmented = rapidcity %>%
  mutate(Summer = ifelse(Month == 6 | Month == 7 | Month == 8,
                         yes = "summer", no = "not_summer"))

# verify mutate function
head(rapidcity_augmented)

# showing a faceted histogram of temperatures by summer status
ggplot(rapidcity_augmented) +
  geom_histogram(aes(x = Temp), binwidth = 2) + 
  facet_wrap(~Summer, nrow = 2)

# arrange

rapidcity %>%
  # Use arrange for sorting on specific variables.
  # arrange(Temp) %>%
  # Arrange in descending order.
  arrange(desc(Temp)) %>%
  head(10)