# Week 43 (2021): Distribution of pumpkin weight by type

library(tidyverse)    # data manipulation
library(ggridges)     # ridgelineplots

# Load tidy tuesday data
pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv',
                            col_types = cols(variety = col_character()))

# Subset columns
pumpkins_subset <- 
  pumpkins %>%
  select(c(id, weight_lbs))

# Recalculate pounds to kilo's and add type of pumpkin
pumpkins_weight <- 
  pumpkins_subset %>%
  mutate_at(vars(weight_lbs), as.numeric) %>%
  filter(!is.na(weight_lbs)) %>%
  mutate(kilo = weight_lbs / 2.2046,
         type = factor(str_extract(id, pattern = "\\w$"),
                       levels = c("T", "L", "F", "P", "S", "W"),
                       labels = c("Tomato", "Long Gourd", "Field Pumpkin", 
                                  "Giant Pumpkin", "Giant Squash",  "Giant Watermelon"),
                       ordered = TRUE)) %>%
  filter(type != "Tomato") %>%
  mutate(type = factor(type))

save(pumpkins_weight,
     file = here::here("Data", "2021-43 Pumpkins"))

# Clean workspace
rm(list = ls())
