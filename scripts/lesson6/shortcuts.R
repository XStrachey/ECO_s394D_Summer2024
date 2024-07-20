library(tidyverse)
# use shortcuts library.
library(mosaic)

titanic = read.csv("data/raw data/titanic.csv", header = TRUE)

# calculate the mean of a variable.
mean(~age, data = titanic)

# calculate a mean stratified by some other grouping variable.
mean(age ~ sex, data = titanic)

# calculate the difference between means.
diffmean(age ~ sex, data = titanic)

# calculate proportions.
# (i.e. where survived == "no")
prop(~survived, data = titanic)

# calculate proportions stratified by other variable.
prop(survived ~ sex, data = titanic)

# calculate the difference of two proportions.
diffprop(survived ~ sex, data = titanic)

# favstats for a collection of multiple summary statistics.
favstats(age ~ sex, data = titanic)