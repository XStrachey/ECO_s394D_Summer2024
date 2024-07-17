library(tidyverse)

aclfest = read.csv('data/raw data/aclfest.csv', header = TRUE)

head(aclfest)

# ~ means “by” or “according to”.
lollapalooza_table = xtabs(~lollapalooza, data = aclfest)

# This function turns a table of counts into a table of proportions.
prop.table(lollapalooza_table)

# Or use the piple %>%
xtabs(~lollapalooza, data = aclfest) %>%
  prop.table() %>%
  round(3)

# Here you should interpret the + sign as meaning "and".
xtabs(~acl + lollapalooza, data = aclfest) %>%
  prop.table() %>%
  round(3)

# xtabs(~acl + lollapalooza + outsidelands, data = aclfest)

xtabs(~bonnaroo, data = aclfest) %>%
  prop.table()

xtabs(~coachella, data = aclfest) %>%
  prop.table()

bonnaroo_and_coachella = xtabs(~bonnaroo + coachella, data = aclfest)

bonnaroo_and_coachella %>%
  prop.table() %>%
  # addmargins adds the probabilities of the individual events
  # in isolation to the margins of the table of joint probabilities.
  addmargins()

acl_and_lollapalooza = xtabs(~acl + lollapalooza, data = aclfest)

acl_and_lollapalooza %>%
  # What margin=2 does is to tell prop.table that 
  # it should calculate proportions conditional on the second variable we named.
  prop.table(margin = 2) %>%
  round(3)

acl_and_lollapalooza %>%
  prop.table(margin = 1) %>%
  round(3)

bonnaroo_and_coachella %>%
  prop.table(margin = 1)

acl_and_outsidelands = xtabs(~acl + outsidelands, data = aclfest)

acl_and_outsidelands %>%
  prop.table(margin=2)