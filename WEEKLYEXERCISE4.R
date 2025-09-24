library(usethis)
git_default_branch()
# main

library(readr)
library(tidyverse)
library(ggplot2)

olympics <- read_csv("Olympics.csv", show_col_types = FALSE)

# (a)
olympics <- olympics %>%
  mutate(total.medals = rowSums(across(c(gold, silver, bronze)), na.rm = TRUE))
print(olympics %>% select(country, year, gold, silver, bronze, total.medals) %>% slice_head(n = 5))

# (b)
gold_by_country <- olympics %>%
  group_by(country) %>%
  summarise(total_gold = sum(gold, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_gold))
print(gold_by_country %>% slice_head(n = 5))

# (c)
medals_by_year <- olympics %>%
  group_by(year) %>%
  summarise(total_medals_given = sum(total.medals, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)
print(medals_by_year)
