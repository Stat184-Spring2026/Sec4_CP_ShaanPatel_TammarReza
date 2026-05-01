rm(list = ls())


library(readxl)
library(tidyverse)

setwd("C:/Users/ND202/OneDrive/Desktop")
food_and_beverages <- read_excel("food and beverages.xlsx", skip = 11)
meat <- read_excel("meat and poultry.xlsx", skip = 11)
fruits_and_veggies <- read_excel("fruits and vegetables.xlsx", skip = 11)
bevs <- read_excel("nonalcoholic beverages.xlsx", skip = 11)

food_and_beverages_long <- food_and_beverages %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

meat_long <- meat %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

fruits_and_veggies_long <- fruits_and_veggies %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

bevs_long <- bevs %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

ggplot(food_and_beverages_long, aes(Date, Index)) +
  geom_line(color = "firebrick", linewidth = 1.2) +
  labs(
    title = "Food & Beverages CPI (2000–2026)",
    x = "Year",
    y = "CPI"
  ) +
  theme_minimal(base_size = 14)

# Helper function to compute increase
compute_increase <- function(df_long, name){
  df_long %>%
    filter(Year %in% c(2000, 2026)) %>%
    group_by(Year) %>%
    summarize(Index = mean(Index, na.rm = TRUE)) %>%
    summarize(
      Category = name,
      Increase = diff(Index)   # 2026 - 2000
    )
}

inc_foodbev  <- compute_increase(food_and_beverages_long, "Food & Beverages")
inc_meat     <- compute_increase(meat_long, "Meat & Poultry")
inc_fruitveg <- compute_increase(fruits_and_veggies_long, "Fruits & Vegetables")
inc_bevs     <- compute_increase(bevs_long, "Nonalcoholic Beverages")

# Combine into one table
increases <- bind_rows(inc_foodbev, inc_meat, inc_fruitveg, inc_bevs)
increases

ggplot(increases, aes(x = Category, y = Increase, fill = Category)) +
  geom_col(width = 0.7) +
  labs(
    title = "Increase in CPI (2000–2026)",
    x = "",
    y = "Increase in price by %"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

restaurants <- read_excel("restaurants.xlsx", skip = 11)
groceries <- read_excel("groceries.xlsx", skip = 11)

restaurants_long <- restaurants %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

groceries_long <- groceries %>%
  select(Year, Jan:Dec) %>%
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Index") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = as.Date(paste(Year, Month, "01", sep = "-"))
  ) %>%
  arrange(Date)

restaurants_long <- restaurants_long %>%
  mutate(Category = "Restaurants")

groceries_long <- groceries_long %>%
  mutate(Category = "Groceries")

combined <- bind_rows(restaurants_long, groceries_long)

ggplot(combined, aes(x = Date, y = Index, color = Category)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Grocery Prices vs Restaurant Prices (2000–2026)",
    x = "Year",
    y = "CPI Index (% Increase",
    color = "Category"
  ) +
  theme_minimal(base_size = 14)



