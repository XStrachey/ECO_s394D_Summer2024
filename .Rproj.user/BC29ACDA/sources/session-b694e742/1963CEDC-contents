# Our goal for this data analysis is to make a line graph showing total toy imports over time, 
# summed across all categories, 
# for the U.S.’s top 3 trading partners by total dollar value of toys imported.

library(tidyverse)

toyimports = read.csv("data/raw data/toyimports.csv", header = TRUE)

country_totals = toyimports %>%
  # Group all the observations by trading partner (the partner_name variable).
  group_by(partner_name) %>%
  # For each partner, calculate total dollar value by summing toy imports (US_report_import).
  summarize(total_dollar_value = sum(US_report_import)) %>%
  # Arrange the partners by total dollar value.
  arrange(desc(total_dollar_value))

head(country_totals)
# A tibble: 6 × 2
# partner_name     total_dollar_value
# <chr>                         <dbl>
#   1 China                     26842305.
# 2 Denmark                    1034990.
# 3 Canada                      572309.
# 4 Hong Kong, China            545186.
# 5 Switzerland                 400969.
# 6 Korea, Rep.                 350612.

# create a list of the top3.
# Here c means “combine”.
top3_partner_names = c('China', 'Denmark', 'Canada')

top3_byyear = toyimports %>%
  # Filter the data set so that it includes only data points from the top 3 trading partners.
  filter(partner_name %in% top3_partner_names) %>%
  # Group the data points by partner and year.
  group_by(year, partner_name) %>%
  # Within each group, sum the toy imports across all categories.
  summarize(yearly_dollar_value = sum(US_report_import))

head(top3_byyear)

ggplot(top3_byyear) +  
  geom_line(aes(x = year, y = yearly_dollar_value, color = partner_name)) +
  # Adjust the color scale more friendly to those with colorblindness.
  scale_color_brewer(type = 'qual') +
  # Plotting the data on a logarithmic axis, 
  # which is a more natural way to compare quantities on very different scales.
  scale_y_log10() +
  scale_x_continuous(breaks = 1996:2005) +
  labs(x = "Year", y = "Dollar value of imports (log scale)",
       title = "Toy imports from the U.S.'s top-3 partners, 1996-2005")