library(tidyverse)

# scatter plots
tvshows = read.csv("data/raw data/tvshows.csv", header = TRUE)

# tells R that you’re going to be plotting data from the tvshows data set.
# "+" is a punctuation mark in the grammar of graphics that means “add a layer.”
ggplot(tvshows) +
  geom_point(aes(x = GRP, y = PE, color = Genre))
  # geom_point(aes(x = GRP, y = PE, shape = Genre))
  # geom_point(aes(x = GRP, y = PE, size = Genre))
  # geom_point(aes(x = GRP, y = PE, alpha = Genre))

ggplot(tvshows) +
  geom_point(aes(x = GRP, y = PE)) +
  # faceting variable
  # Faceting variables are always categorical 
  # (although you can also facet on a numerical variable if you discretize it first).
  facet_wrap(~Genre)

# line plots
power_christmas2015 = read.csv("data/raw data/power_christmas2015.csv", header = TRUE)

ggplot(power_christmas2015) +
  geom_line(aes(x = hour, y = ERCOT))

# histograms plots
rapidcity = read.csv("data/raw data/rapidcity.csv", header = TRUE)

ggplot(rapidcity) +
  # we can also exercise finer control over the bin width
  geom_histogram(aes(x = Temp), binwidth = 3) +
  # You can also tell R how how many rows you want in your faceted plot.
  facet_wrap(~Month, nrow = 12)

# we can interpret the histogram as an approximate probability distribution.
ggplot(rapidcity) +
  # The double-dots in y=..density.. are there to inform R 
  # that density is not actually a variable in the data set
  geom_histogram(aes(x = Temp, y = ..density..), binwidth = 3)

# boxplots plot
# The difference between a boxplot and a histogram is that, 
# while a histogram tries to show the full shape of a data distribution, 
# a boxplot shows only a summary of that distribution, 
# in the form of a little cartoon with a box and whiskers.
kroger = read.csv("data/raw data/kroger.csv", header = TRUE)

ggplot(kroger) +
  # By default, whiskers extend only 1.5 times the height of the box itself, 
  # with points falling outside this range shown individually as dots.
  geom_boxplot(aes(x = city, y = vol))

# Jitter plots
# A jitter plot shows each data point individually above a label for its corresponding group.
kroger_dallas = kroger %>%
  # We’ll use filter to pull out these observations into a separate data frame.
  filter(city == 'Dallas')

ggplot(kroger_dallas) +
  # The points don’t line up exactly above the the “yes” or “no” labels 
  # because we added a bit of meaningless random “jitter” to the dots, 
  # which allows the eye to distinguish the individual cases more easily.
  # The width parameter controls how much jitter is added.
  geom_jitter(aes(x = disp, y = vol), width = 0.1)

# bar plots
car_class_summaries = read.csv("data/raw data/car_class_summaries.csv", header = TRUE)

ggplot(car_class_summaries) +
  geom_col(aes(x = class, y = average_cty))

ggplot(car_class_summaries) +
  geom_col(aes(x = class, y = prop_4cyl))

# geom_bar vs geom _col
# If you want to make a bar plot of counts, 
# and you need R to do the counting for you, use geom_bar.
# If using geom_col, we should have a table of pre-computed summary statistics.

# Load tidyverse built-in data set.
data(mpg)

ggplot(car_class_summaries) +
  geom_col(aes(x = class, y = n))

ggplot(mpg) +
  geom_bar(aes(x = class))

# customizing plots
# https://statisticsglobe.com/change-font-size-of-ggplot2-plot-in-r-axis-text-main-title-legend

ggplot(tvshows) + 
  geom_point(aes(x = GRP, y = PE, color = Genre)) + 
  # https://colorbrewer2.org/#type=qualitative&scheme=Accent&n=3
  scale_color_brewer(type = "qual")

# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
library(viridis)  

ggplot(tvshows) + 
  geom_point(aes(x = GRP, y = PE, color = Genre)) + 
  scale_color_viridis(discrete = TRUE)

ggplot(kroger) +
  geom_boxplot(aes(x = city, y = vol)) +
  labs(x = "Location of store",
       y = "Weekly sales volume (packages sold)",
       title = "Weekly cheese sales at 11 U.S. Kroger stores") +
  theme(axis.text = element_text(size = 8)) +
  # You can flip the coordinate axes in any ggplot using coord_flip
  coord_flip()