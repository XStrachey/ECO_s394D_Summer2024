# Q1: Subjects, objects, and verbs are three key elements in the grammar of English. 
# What are the three key elements in the grammar of graphics?
# 1. variables 2. objects 3. mappings

# Q2
# I think using a line graph is more effective for conveying the desired information. 
# This is because “across the day” inherently implies temporal flow, 
# where a line graph is better suited compared to a bar plot. 
# Additionally, presenting working vs. non-working days as two lines on the same chart 
# allows readers to directly compare their distributions, 
# which is more straightforward compared to a faceted bar plot.

# Q3
library(tidyverse)

gapminder2007 = read.csv("data/raw data/gapminder2007.csv", header = TRUE)

# A
ggplot(gapminder2007) +
  geom_point(aes(x = gdpPercap, y = lifeExp))

# B
ggplot(gapminder2007) +
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent))

# C
ggplot(gapminder2007) +
  geom_point(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))