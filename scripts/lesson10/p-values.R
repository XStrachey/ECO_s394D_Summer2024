library(tidyverse)
library(mosaic)

# simulate one time flipping coin
rflip(25)

# simulate 10000 times flipping coins
# patriots_sim, is a data frame with a single column, called nflip
patriots_sim = do(10000)*nflip(25)

ggplot(patriots_sim) + 
  geom_histogram(aes(x = nflip), binwidth = 1)

sum(patriots_sim >= 19)

# Example 2: a disease cluster?

# Step 1: Our null hypothesis is children near nuclear power plants in 
# Illinois experience leukemia at the background rate of 4.7 per 10,000, on average over the long run.

# Step 2: Our test statistic is the number of leukemia cases. 
# Higher numbers of cases imply stronger evidence against the null hypothesis. 
# In our data, 47 of 80,515 children living near nuclear plants had leukemia between 1996 and 2005.

# Step 3: we must calculate the probability distribution of the test statistic, 
# assuming that the null hypothesis is true.

# 0.00047 is the base rate of leukemia among Illinois children 
# living more than 30 miles from a nuclear plant. 
# Remember, our null hypothesis assumes that all children experience 
# the same average rate of leukemia, regardless of their proximity to a nuclear power plant.
sim_cancer = do(10000)*nflip(n = 80515, prob = 0.00047)

ggplot(sim_cancer) + 
  geom_histogram(aes(x = nflip), binwidth = 1)

# step 4: calculate a p-value. How many simulations yielded 47 cases of cancer or more, just by chance?
sum(sim_cancer >= 47)
# The results show that children living within 10 miles of any nuclear power plant 
# did not have significant increase in incidence for leukemia, lymphomas, or other cancer sites.

# Essential:
# Null hypothesis: the hypothesis of no effect.
# Test statistic: a numerical summary of the data that measures the evidence against the null hypothesis.
# p-value: the probability of observing a test statistic at least as extreme as 
# what we actually observed, just by chance, if the null hypothesis were true.