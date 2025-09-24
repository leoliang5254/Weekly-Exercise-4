library(usethis)
git_default_branch()
# main
library(readr)
library(tidyverse)
library(ggplot2)

olympics <- read_csv("Olympics.csv", show_col_types = FALSE)

# 6
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

#7
# (a)
largest_1992 <- olympics %>%
  filter(year == 1992) %>%
  select(country, athletes) %>%
  arrange(desc(athletes))

print(largest_1992)
# United States had the largest delegation of athletes in 1992

# (b)
selected_countries <- c("United States", "France", "Germany", "Russia", "China")

gold_plot <- olympics %>%
  filter(country %in% selected_countries) %>%
  group_by(year, country) %>%
  summarise(total_gold = sum(gold, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_gold, color = country)) +
  geom_line() +
  geom_point() +
  labs(x = "Year",
       y = "Gold Medals",
       color = "Country") +
  theme_minimal()

print(gold_plot)
