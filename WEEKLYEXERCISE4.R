library(usethis)
git_default_branch()
# main
library(readr)
library(tidyverse)
library(ggplot2)

olympics <- read_csv("Olympics.csv", show_col_types = FALSE)

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
