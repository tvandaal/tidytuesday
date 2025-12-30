# Week 20 (2024): Coffee

# Load packages ====
library(tidyverse)
library(here)
library(patchwork)

# Load data ====
coffee_survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Get overview of data ====
glimpse(coffee_survey)
skimr::skim(coffee_survey)

# Select variables and remove rows only NA's ====
coffee_clean <- 
  coffee_survey %>%
  select(
    expertise,
    starts_with("coffee"),
    starts_with("prefer")
  ) %>%
  filter(if_any(everything(), ~ !is.na(.)))


# Check distribution of ratings and preferences

coffee_clean %>%
  select(-c(ends_with("notes"), starts_with("prefer"), expertise)) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  group_by(name) %>%
  count(value) %>%
  ggplot(
    aes(x = value, y = n)
  ) +
  geom_col() +
  facet_wrap(~name, nrow=5) +
  theme_minimal()

coffee_clean %>%
  select(starts_with("prefer")) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  group_by(name) %>%
  count(value) %>%
  ggplot(
    aes(x = value, y = n)
  ) +
  geom_col() +
  facet_wrap(~name, nrow=4) +
  theme_minimal()

coffee_clean %>%
  select(expertise) %>%
  pivot_longer(
    cols = everything()
  ) %>%
  group_by(name) %>%
  count(value) %>%
  ggplot(
    aes(x = value, y = n)
  ) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = "bottom", color = "steelblue")+
  scale_x_continuous(
    breaks = seq(1, 10, 1),
    name = "rating"
  ) +
  scale_y_continuous(limits = c(0,985))+
  coord_cartesian(expand=F) +
  theme_void() +
  theme(
    axis.title.x = element_text(),
    axis.text.x = element_text()
    )

# Visualisations per coffee ====
# 1: bars representing number of times preferred 
# 2: barplot of expertise by preferred coffee
# 3: heatmap of ratings by criteria per coffee
# x = rating, y= criteria, fill = heat

## 1: bars representing number of times each coffee is preferred  ====

plot_1 <-
  coffee_clean %>%
  filter(!is.na(prefer_overall)) %>%
  count(prefer_overall) %>%
  ggplot(
    aes(
      x = prefer_overall,
      y = n
      ) 
    ) +
  geom_col(fill = "#38220f") +
  geom_text(
    aes(label = n - 25),
    color = "white",
    vjust = "top"
  ) +
  scale_y_continuous(
    limits = c(0, 1600)
  ) +
  facet_wrap(
    ~prefer_overall, scales = "free_x", nrow = 1
    ) +
  theme_void() +
  theme(
    strip.text = element_text(
      size = 12, face = "bold"
    ) 
  )


## 2: barplot of expertise by preferred coffee ====

plot_2 <-
  coffee_clean %>%
  filter(!is.na(prefer_overall)) %>%
  group_by(prefer_overall) %>%
  count(expertise) %>%
  ggplot(
    aes(
      x = expertise,
      y = n
    ) 
  ) +
  geom_col(fill = "#38220f") +
  scale_x_continuous(
    breaks = seq(1, 10, 1)
    ) +
  facet_wrap(
    ~prefer_overall, scales = "free_x", nrow = 1
  ) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    axis.text.x = element_text(size = 8)
  )

# 3: heatmap of ratings by criteria per coffee

plot_3 <-
  coffee_clean %>%
  filter(!is.na(prefer_overall)) %>%
  select(
    prefer_overall,
    contains("acidity"),
    contains("bitterness")
  ) %>%
  pivot_longer(
    cols = c(2:9)
    ) %>%
  group_by(name, prefer_overall) %>%
  count(value) %>%
  mutate(
    criterium = ifelse(
      str_detect(name, "acidity"),
      yes = "Acidity",
      no = "Bitterness"
    )
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(
    aes(
      x = value,
      y = criterium,
      fill = n
    ) 
  ) +
  geom_tile() +
  scale_x_continuous(
    breaks = seq(1, 5, 1),
    position = "top"
  ) +
  facet_wrap(
    ~prefer_overall, nrow = 1
  ) +
  scale_fill_steps2(
    low = "#ece0d1",
    high = "#38220f"
  ) +
  coord_equal(expand=FALSE) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    axis.text.x = element_text(size = 8),
    legend.position = "none"
  )

plot_1/plot_2/plot_3

"#ece0d1"
"#dbc1ac"
"#967259"
"#634832"
"#38220f"