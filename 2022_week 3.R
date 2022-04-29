# Week 3 (2022): Chocolate

# Load packages
library(tidyverse)
library(treemapify)
library(ggtext)

# Load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# Select top 10 producing countries of chocolate
chocolate %>% 
  ggplot(aes(x = fct_rev(fct_infreq(company_location)))) +
  geom_bar(color = "#3C1321", fill = "#3C1321") +  #623412
  scale_y_continuous(breaks = c(seq(0, 1000, 250), 
                                max(chocolate %>% 
                                      group_by(company_location) %>%
                                      count() %>% pull(n))),
                     position = "right", 
                     name = "Number of chocolate companies",
                     expand = c(0,0)) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "News Cycle"),
        plot.title = element_text(family = "News Cycle Bold", face = "bold"),
        axis.text.x = element_text(size = 10,),
        axis.text.y = element_text(size = 5),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 0, r = 1, b = 0, l = 0, "cm"))

# Tidy data and only select data of top ten producing countries
chocolate_tidy <- 
  chocolate %>%
  filter(company_location %in% c("U.S.A.", "Canada", "France", "U.K.", "Italy",
                                 "Belgium", "Ecuador", "Australia", "Switzerland", "Germany")) %>%
  mutate(region = factor(company_location),
         cocoa_perc = parse_number(cocoa_percent),
         n_ingredients = parse_number(ingredients),
         beans = ifelse(str_detect(ingredients, "B"),
                        yes = 1, no = 0),
         sugar = ifelse(str_detect(ingredients, "S$"),
                        yes = 1, no = 0),
         sweetener = ifelse(str_detect(ingredients, "S*$"),
                            yes = 1, no = 0),
         beans = ifelse(str_detect(ingredients, "B"),
                        yes = 1, no = 0),
         cocoa_butter = ifelse(str_detect(ingredients, "C"),
                               yes = 1, no = 0),
         vanilla = ifelse(str_detect(ingredients, "V"),
                          yes = 1, no = 0),
         lecithin = ifelse(str_detect(ingredients, "L"),
                           yes = 1, no = 0),
         salt = ifelse(str_detect(ingredients, "Sa"),
                       yes = 1, no = 0)) %>%
  separate(most_memorable_characteristics, 
           into = paste0("characteristic", c(1:4)),
           sep = ",",
           remove = FALSE) %>%
  mutate_at(vars(ref, company_manufacturer,
                 country_of_bean_origin, specific_bean_origin_or_bar_name), factor) %>%
  select(-c(company_location, most_memorable_characteristics, ingredients, cocoa_percent))
         
# Overview of data         
skimr::skim(chocolate_tidy)         

# Create data for making a treeplot on number of ingrediënts 
chocolate_plot <- chocolate_tidy %>%
  group_by(region, n_ingredients) %>%
  count() %>%
  ungroup() %>%
  mutate(n_ingredients = replace_na(n_ingredients, replace = 0),
         label_ingredients = factor(case_when(n_ingredients == 0 ~ "not known",
                                              n_ingredients == 1 ~ "1 ingredient",
                                              n_ingredients == 2 ~ "2 ingredients",
                                              n_ingredients == 3 ~ "3 ingredients",
                                              n_ingredients == 4 ~ "4 ingredients",
                                              n_ingredients == 5 ~ "5 ingredients"))) 

# Create treemap 
chocolate <- 
  ggplot(chocolate_plot,
       aes(area = n, 
           fill = region,
           label = label_ingredients,
           subgroup = region)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white") +
  geom_treemap_text(aes(alpha = n_ingredient),
                    fontface = "italic",
                    colour = "white",
                    place = "center",
                    size = 8,
                    alpha = .5,
                    grow = F,
                    family = "News Cycle",
                    reflow = T) +
  geom_treemap_subgroup_text(place = "topleft",
                             fontface = "bold",
                             grow = F,
                             size = 10,
                             colour = "black",
                             family = "News Cycle",
                             min.size = 0) +
  scale_fill_manual(values = c("#947B4F", "#947B4F", "#80471C", "#947B4F", "#80471C",
                               "#947B4F", "#947B4F", "#947B4F", "#947B4F", "#4B371C")) +
  labs(title = "<span style = 'font-size:14pt;color:#964B00;'><b>Number of chocolate companies per country by number of ingredients</b></span style><br>
       <span style = 'font-size:10pt;color:#964B00;'>The U.S.A. harvest over 1000 chocolate companies. These factories mostly use two or three ingredients in producing this chocolate.",
       caption = "<br>Data source: flavorsofchocolate.com | #tidytuesday | @TinevanDaal") +
  theme_minimal() +
  theme(plot.title = element_textbox(),
        plot.caption = element_markdown(face = "italic", size = 9, color = "grey80", hjust = 0),
        legend.position = "none")

# Save plot
png_file <- here::here("Figures", "2022-03 Chocolate.png")
pdf_file <- here::here("Figures", "2022-03 Chocolate.pdf")

ggsave(
  png_file,
  device = png,
  width = 21, 
  height = 21, 
  units  = "cm"
)

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 21, 
  height = 21, 
  units  = "cm"
)

