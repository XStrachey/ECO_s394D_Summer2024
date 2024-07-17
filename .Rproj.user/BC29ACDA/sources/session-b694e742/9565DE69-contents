# Study questions
library(tidyverse)

aclfest = read.csv('data/raw data/aclfest.csv', header = TRUE)

head(aclfest)

# Question 1
# What is the joint probability that a band played both Outside Lands and Bonnaroo?
outsidelands_and_bonnaroo = xtabs(~outsidelands + bonnaroo, data = aclfest)

outsidelands_and_bonnaroo %>%
  prop.table() %>%
  round(3) %>%
  addmargins()

# Output:
#     bonnaroo
# outsidelands     0     1   Sum
# 0   0.611 0.234 0.845
# 1   0.129 0.026 0.155
# Sum 0.740 0.260 1.000

# What is the conditional probability that a band played Coachella, given that they played ACL Fest?
coachella_and_acl = xtabs(~coachella + acl, data = aclfest)

coachella_and_acl %>%
  prop.table(margin = 2) %>%
  round(3)

# Output:
#   acl
# coachella     0     1
# 0 0.546 0.608
# 1 0.454 0.392

# Question 2
plays_top50 = read.csv('data/raw data/plays_top50.csv', header = TRUE)

head(plays_top50)

# For a randomly selected user, 
# what are P(plays Franz Ferdinand) and P(plays Franz Ferdinand | plays the Killers)?
xtabs(~franz.ferdinand, data = plays_top50) %>%
  prop.table()

xtabs(~franz.ferdinand + the.killers, data = plays_top50) %>%
  prop.table(margin = 2)

# What are P(plays Bob Dylan) and P(plays Bob Dylan | plays the Beatles)?
xtabs(~bob.dylan, data = plays_top50) %>%
  prop.table()

xtabs(~bob.dylan + the.beatles, data = plays_top50) %>%
  prop.table(margin = 2)

# What are P(plays Rihanna) and P(plays Rihanna | plays Kanye West)?
xtabs(~rihanna, data = plays_top50) %>%
  prop.table()

xtabs(~rihanna + kanye.west, data = plays_top50) %>%
  prop.table(margin = 2)

# Output:
#                 kanye.west
# rihanna         0         1
# 0          0.9650260 0.8387097
# 1          0.0349740 0.1612903

# Are the events “plays Rihanna” and “plays Kanye West” independent?
# Because P(plays Rihanna | plays Kanye West) = 0.1612903
# and P(plays Rihanna | not plays Kanye West) = 0.0349740
# Clearly, P(plays Rihanna | plays Kanye West) ~= P(plays Rihanna | not plays Kanye West)
# So, the events “plays Rihanna” and “plays Kanye West” independent!

# What is P(plays Queen or plays David Bowie)?
queen_and_david_bowie = xtabs(~queen + david.bowie, data = plays_top50)

queen_and_david_bowie %>%
  prop.table(margin = 2)

# Output:
#                david.bowie
# queen          0          1
# 0         0.92997481 0.82533937
# 1         0.07002519 0.17466063
# So the events "plays Queen" and "plays David Bowie" independent!

queen_and_david_bowie %>%
  prop.table() %>%
  addmargins()

# Output:
#                     david.bowie
# queen          0          1        Sum
# 0        0.86146667 0.06080000 0.92226667
# 1        0.06486667 0.01286667 0.07773333
# Sum      0.92633333 0.07366667 1.00000000
# So P(plays Queen or plays David Bowie) = P(plays Queen) + P(plays David Bowie) -P(plays Queen and plays David Bowie)
# So P(plays Queen or plays David Bowie) = 0.07773333 + 0.07366667 - 0.01286667

p_queen_or_david_bowie = 0.07773333 + 0.07366667 - 0.01286667

# Output:
# 0.1385333
# So P(plays Queen or plays David Bowie) = 0.1385333