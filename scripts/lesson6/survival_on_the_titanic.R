# For this example, we’ll answer the question: 
# how did survival among adult passengers vary by sex and cabin class?

library(tidyverse)

titanic = read.csv("data/raw data/titanic.csv", header = TRUE)

head(titanic)

surv_adults = titanic %>%
  # create a new variable, which we’ll call Adult, 
  # that determines whether a passenger is at least 18 years old.
  mutate(age_bracket = ifelse(age >= 18,
                              yes = "adult", no = "child")) %>%
  # filter the data set down to adults only.
  filter(age_bracket == "adult") %>%
  # group the filtered data set by sex and cabin class.
  group_by(sex, passengerClass) %>%
  summarize(total_count = n(),
            surv_count = sum(survived == 'yes'),
            # calculate the survival percentage for each group.
            surv_pct = surv_count / total_count)

# create a faceted bar plot.
ggplot(surv_adults) +
  geom_col(aes(x = sex, y = surv_pct)) +
  facet_wrap(~passengerClass, nrow = 1)