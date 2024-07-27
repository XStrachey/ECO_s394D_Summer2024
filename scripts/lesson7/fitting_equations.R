library(tidyverse)
library(mosaic)

heartrate = read.csv("data/raw data/heartrate.csv", header = TRUE)

head(heartrate)

# draw scatter plot with regression
ggplot(heartrate) +
  geom_point(aes(x = age, y = hrmax)) +
  # To tell R to add a smooth trend line to a plot
  # method=lm tells R to fit the trend line using a linear model (lm).
  geom_smooth(aes(x = age, y = hrmax), method = 'lm')

# make code more concise

# The general rule here is that, if you place aes inside the call the ggplot
ggplot(heartrate, aes(x = age, y = hrmax)) +
  # , then all subsequent layers will inherit that same aesthetic mapping.
  geom_point() +
  geom_smooth(method = 'lm')

# print line model
model_hr = lm(hrmax ~ age, data = heartrate)
# prints the weights (“coefficients”) of the fitted model to the console.
coef(model_hr)
# (Intercept)         age 
# 207.9306683  -0.6878927 

# print the fitted values for the first five people in our data set.
fitted(model_hr) %>%
  head(5)

heartrate_test = read.csv("data/raw data/heartrate_test.csv", header = TRUE)

# make predictions based on fitted model
# This newdata input must have a column in it that exactly matches 
# the name of features used in your original fitted model.
predict(model_hr, newdata = heartrate_test)

heartrate_test <- heartrate_test %>%
  # The dot (.) is just shorthand for “use the thing piped in as the new data.”
  mutate(hr_pred = predict(model_hr, newdata = .))

# calculates the residuals, or model errors
heartrate <- heartrate %>%
  mutate(hr_residual = resid(model_hr))

# sorting by the residuals in descending order
heartrate %>%
  arrange(desc(hr_residual))

sd(resid(model_hr))
# 7.514252

# calculate the standard deviation of our model’s fitted values
sd(fitted(model_hr))
# 7.754332

# calculate R^2
rsquared(model_hr)

# beyond line model
# Exponential models
# Example 1: Ebola in West Africa
ebola = read.csv("data/raw data/ebola.csv", header = TRUE)

ggplot(ebola) +
  geom_line(aes(x = Day, y = totalSus))

# total cases over time: logarithm scale for y variable
ggplot(ebola) +
  geom_line(aes(x = Day, y = log(totalSus))) +
  geom_abline(intercept = 4.54, slope = 0.0216, color = 'red')

lm_ebola = lm(log(totalSus) ~ Day, data = ebola)
coef(lm_ebola)
# (Intercept)         Day 
# 4.53843517  0.02157854 

# Example 2: Austin’s population over time
austin_metro = read.csv("data/raw data/austin_metro.csv", header = TRUE)

# here’s a plot of Austin’s population over time
ggplot(austin_metro) +
  geom_line(aes(x = Year, y = Population))

austin_metro <- austin_metro %>%
  mutate(YearsSince1840 = Year - 1840)

ggplot(austin_metro) +
  geom_line(aes(x = YearsSince1840, y = log(Population))) +
  geom_abline(intercept = 7.14, slope = 0.04125, color = 'red')

lm_austin_metro = lm(log(Population) ~ YearsSince1840, data = austin_metro)
coef(lm_austin_metro)
# (Intercept) YearsSince1840 
# 7.13896689     0.04125391 

# Power laws
# Example 1: brain versus body weight
animals = read.csv("data/raw data/animals.csv", header = TRUE)

# compute brain to body weight ratio
animals = animals %>%
  mutate(brain_body_ratio = round(brain / body, 2))

ggplot(animals) +
  geom_point(aes(x = body, y = brain))

# The log transformation of both axes has stretched the lower left corner 
# of the box out in both x and y directions.
ggplot(animals) +
  geom_point(aes(x = log(body), y = log(brain))) +
  geom_abline(intercept = 2.2, slope = 0.75, color = 'red')

lm_critters = lm(log(brain) ~ log(body), data = animals)
coef(lm_critters)
# (Intercept)   log(body) 
# 2.2049405   0.7500543 

# add the residuals to the data frame
animals = animals %>%
  mutate(logbrain_residual = resid(lm_critters))

animals %>%
  arrange(desc(logbrain_residual)) %>%
  head()

# Example 2: demand for milk
milk = read.csv("data/raw data/milk.csv", header = TRUE)

ggplot(milk) +
  geom_point(aes(x = price, y = sales))

ggplot(milk) +
  geom_point(aes(x = log(price), y = log(sales))) +
  geom_abline(intercept = 4.72, slope = -1.619, color = 'red')

# let’s fit a linear model for log(sales) versus log(price)
lm_milk = lm(log(sales) ~ log(price), data = milk)
coef(lm_milk)