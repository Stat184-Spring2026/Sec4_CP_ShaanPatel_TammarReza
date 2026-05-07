```{r, echo=TRUE, eval=FALSE}
# Style Guide: Tidyverse Style Guide
# Code Header
# Author: Shaan Patel Tammar Reza and Logan Howard

library(readxl)
library(tidyverse)
library(knitr)
library(scales)

make_long_cpi <- function(file_name, category_name) {
  read_excel(file_name, skip = 11) %>%
    select(Year, Jan:Dec) %>%
    pivot_longer(
      cols = Jan:Dec,
      names_to = "Month",
      values_to = "Index"
    ) %>%
    mutate(
      Month = match(Month, month.abb),
      Date = as.Date(paste(Year, Month, "01", sep = "-")),
      Category = category_name
    ) %>%
    arrange(Date)
}

food_and_beverages_long <- make_long_cpi(
  "Food and beverages.xlsx",
  "Food & Beverages"
)

meat_long <- make_long_cpi(
  "Meat and poultry.xlsx",
  "Meat & Poultry"
)

fruits_and_veggies_long <- make_long_cpi(
  "Fruits and Vegetables.xlsx",
  "Fruits & Vegetables"
)

bevs_long <- make_long_cpi(
  "Nonalcoholic beverages.xlsx",
  "Nonalcoholic Beverages"
)

restaurants_long <- make_long_cpi(
  "Restaurants.xlsx",
  "Restaurants"
)

groceries_long <- make_long_cpi(
  "Groceries.xlsx",
  "Groceries"
)

food_categories <- bind_rows(
  food_and_beverages_long,
  meat_long,
  fruits_and_veggies_long,
  bevs_long
)

combined_food_location <- bind_rows(
  groceries_long,
  restaurants_long
)

ggplot(combined_food_location, aes(x = Date, y = Index, color = Category)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Grocery Prices vs Restaurant Prices",
    x = "Year",
    y = "CPI Index",
    color = "Category"
  ) +
  theme_minimal(base_size = 14)

ggplot(food_categories, aes(x = Date, y = Index, color = Category)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Food Category CPI Trends Over Time",
    x = "Year",
    y = "CPI Index",
    color = "Category"
  ) +
  theme_minimal(base_size = 14)

compute_percent_increase <- function(df_long) {
  df_long %>%
    filter(Year %in% c(2000, 2025)) %>%
    group_by(Category, Year) %>%
    summarize(Index = mean(Index, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Year, values_from = Index) %>%
    mutate(
      `Percent Increase` = (`2025` - `2000`) / `2000` * 100
    )
}

category_increases <- compute_percent_increase(food_categories)

kable(
  category_increases,
  digits = 1,
  caption = "Percent increase in CPI by food category from 2000 to 2025."
)

ggplot(category_increases, aes(x = reorder(Category, `Percent Increase`), 
                               y = `Percent Increase`, 
                               fill = Category)) +  # 1. Added fill here
  geom_col(width = 0.7) +                           # 2. Keep this clean
  coord_flip() +
  labs(
    title = "Percent Increase in Food Category CPI",
    x = "Food Category",
    y = "Percent Increase"
  ) +
  theme_minimal(base_size = 14)
```
