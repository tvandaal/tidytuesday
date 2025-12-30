# Week 5 (2023): Cats

# Load packages ====
library(tidyverse)
library(here)


# Load data ====
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')


# Get overview of data ====
glimpse(cats_uk_reference)
skimr::skim(cats_uk_reference)


# Tidy data ====
cats_clean <-
  cats_uk_reference %>%
  select(-c(tag_id, animal_taxon, study_site, starts_with("deploy"))) %>%
  mutate_at(vars(c(prey_p_month, hrs_indoors, n_cats, age_years)), as.numeric) %>%
  mutate_at(vars(animal_id, animal_sex), factor) %>%
  rowwise() %>%
  mutate(food_type = case_when(
    all(is.na(c_across(starts_with("food")))) ~ NA_character_,
    food_dry & food_wet & food_other ~ "dry_wet_other",
    food_dry & food_wet ~ "dry_wet",
    food_dry & food_other ~ "dry_other",
    food_wet & food_other ~ "wet_other",
    food_dry ~ "dry",
    food_wet ~ "wet",
    food_other ~ "other",
    TRUE ~ NA_character_
  )) %>%
  ungroup() %>%
  mutate(
    can_hunt = ifelse(hunt == TRUE, yes = "Allowed to hunt", no = "Not allowed to hunt"),
    sex = ifelse(animal_sex == "m", yes = "male", no = "female"),
    hunt_sex = interaction(can_hunt, sex),
    id = 1: n()
  ) %>%
  select(-c(food_dry, food_wet, food_other))

table(cats_clean$sex, cats_clean$animal_sex, useNA = "ifany")
table(cats_clean$can_hunt, cats_clean$hunt, useNA = "ifany")

# Add lines to data set to create empty spaces
empty_bar <- 5

to_add <- data.frame(matrix(NA, empty_bar * nlevels(cats_clean$hunt_sex), ncol(cats_clean)))
colnames(to_add) <- colnames(cats_clean)
to_add$hunt_sex <- rep(levels(cats_clean$hunt_sex), each=empty_bar)
plot_data <- rbind(cats_clean, to_add)
plot_data <- plot_data %>% arrange(hunt_sex) %>%
  mutate(id = seq(1, nrow(plot_data)))


# Create data with labels for circular barplot ====
label_data <- plot_data

number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id - 0.5) / number_of_bar     

label_data$hjust <- ifelse(angle < -90, yes = 1, no = 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)


# Create plot
cats_clean %>%
  ggplot(aes(x = as.factor(id), y = prey_p_month, fill = hunt_sex)) +
  geom_bar(stat = "identity") +
  ylim(-100,100) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  )  +
  geom_text(
    data = label_data, 
    aes(x = id, y = prey_p_month + 20, label = animal_id, hjust = hjust),
    color = "black", 
    fontface ="bold",
    alpha = 0.6, 
    size = 2.5, 
    angle = label_data$angle, 
    inherit.aes = FALSE 
    ) +
  coord_polar(start = 0)
