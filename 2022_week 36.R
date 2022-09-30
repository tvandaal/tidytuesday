# Week 36 (2022): Lego

# Load packages ====
library(tidyverse)
library(here)
library(ggtext)

# Load data ====
lego_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

# Get overview of data ====
glimpse(lego_sets)
skimr::skim(lego_sets)

# Create to plot bricks in background
common_lego_colors <- c("#F12D3E", "#0055AD", "#F6B800", "#00704A")

background_data1 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(0, 8),
         ymax = rep(100, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 0, ymax = 100, year=NA,
          color = sample(common_lego_colors, 1))
background_data2 <- 
  tibble(xmin = c(seq(1955, 2015, 10)),
         xmax = ifelse(xmin != 2015, yes = xmin+9.99, no = 2021.99),
         ymin = rep(100.01, 7),
         ymax = rep(200, 7),
         year = rep(NA, 7),
         color = sample(common_lego_colors, 7, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1954.99,
          ymin = 100.01, ymax = 200, year=NA,
          color = sample(common_lego_colors, 1))
background_data3 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(200.01, 8),
         ymax = rep(300, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 200.01, ymax = 300, year=NA,
          color = sample(common_lego_colors, 1))
background_data4 <- 
  tibble(xmin = c(seq(1955, 2015, 10)),
         xmax = ifelse(xmin != 2015, yes = xmin+9.99, no = 2021.99),
         ymin = rep(300.01, 7),
         ymax = rep(400, 7),
         year = rep(NA, 7),
         color = sample(common_lego_colors, 7, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1954.99,
          ymin = 300.01, ymax = 400, year=NA,
          color = sample(common_lego_colors, 1))
background_data5 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(400.01, 8),
         ymax = rep(500, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 400.01, ymax = 500, year=NA,
          color = sample(common_lego_colors, 1))
background_data6 <- 
  tibble(xmin = c(seq(1955, 2015, 10)),
         xmax = ifelse(xmin != 2015, yes = xmin+9.99, no = 2021.99),
         ymin = rep(500.01, 7),
         ymax = rep(600, 7),
         year = rep(NA, 7),
         color = sample(common_lego_colors, 7, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1954.99,
          ymin = 500.01, ymax = 600, year=NA,
          color = sample(common_lego_colors, 1))
background_data7 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(600.01, 8),
         ymax = rep(700, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 600.01, ymax = 700, year=NA,
          color = sample(common_lego_colors, 1))
background_data8 <- 
  tibble(xmin = c(seq(1955, 2015, 10)),
         xmax = ifelse(xmin != 2015, yes = xmin+9.99, no = 2021.99),
         ymin = rep(700.01, 7),
         ymax = rep(800, 7),
         year = rep(NA, 7),
         color = sample(common_lego_colors, 7, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1954.99,
          ymin = 700.01, ymax = 800, year=NA,
          color = sample(common_lego_colors, 1))
background_data9 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(800.01, 8),
         ymax = rep(900, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 800.01, ymax = 900, year=NA,
          color = sample(common_lego_colors, 1))
background_data10 <- 
  tibble(xmin = c(seq(1955, 2015, 10)),
         xmax = ifelse(xmin != 2015, yes = xmin+9.99, no = 2021.99),
         ymin = rep(900.01, 7),
         ymax = rep(1000, 7),
         year = rep(NA, 7),
         color = sample(common_lego_colors, 7, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1954.99,
          ymin = 900.01, ymax = 1000, year=NA,
          color = sample(common_lego_colors, 1))
background_data11 <- 
  tibble(xmin = seq(1950, 2020, 10),
         xmax = ifelse(xmin < 2011, yes = xmin+9.99, no = 2022),
         ymin = rep(1000.01, 8),
         ymax = rep(1092, 8),
         year = rep(NA, 8),
         color = sample(common_lego_colors, 8, replace = T)) %>%
  add_row(.before=1,  xmin = 1949, xmax = 1949.99,
          ymin = 1000.01, ymax = 1092, year=NA,
          color = sample(common_lego_colors, 1))

# Create Plot
lego_plot <- 
  lego_sets %>%
  group_by(year) %>%
  count() %>%
  ggplot() +
  geom_rect(data = background_data1,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data2,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data3,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
             alpha = .5) +
  geom_rect(data = background_data4,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data5,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data6,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data7,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data8,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data9,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data10,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_rect(data = background_data11,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = color, color = color),
            alpha = .6) +
  geom_line(aes(x = year, y = n), size = 2) +
  labs(title = "<span style='font-size:15pt;'>Lego releases more and more sets over the year.</span style><br><span style='font-size:9pt;'><i>The chart shows the number of lego sets released between 1949 and 2021. Colors of the bricks are randomly picked and do not have a meaning.<br>", 
       caption = "Data source: rebrickable.com | #tidytuesday | @TinevanDaal",
       y = "Number of lego sets released") +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(add=c(0,1)),
                     breaks = seq(1950, 2020, 10),
                     position = "top") +
  scale_y_continuous(expand = expansion(add=c(0,0)),
                     breaks = seq(0, 1000, 200),
                     limits = c(0,1093))+
  theme_minimal() +
  theme(text = element_text(family = "Glegoo"),
        plot.title = element_textbox_simple(face = "bold", lineheight=1),
        plot.title.position = "plot",
        plot.caption = element_text(size = 5, color = "grey80"),
        plot.margin = margin(0,0,.5, 0, "cm"),
        axis.title.y = element_text(size = 9, color = "grey80"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 7, color = "grey80"),
        panel.grid = element_blank(),
        axis.title.x = element_blank())

# Save plot ====
pdf_file <- here("Figures", "2022-36 Lego.pdf")

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 22, 
  height = 16,
  unit = "cm"
)



rm(list = ls())

