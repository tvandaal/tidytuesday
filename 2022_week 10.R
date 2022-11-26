# Week 10 (2022): Erasmus

# Load packages ====
library(tidyverse)
library(here)
library(countrycode)
library(ggtext)
library(patchwork)

# Load data ====
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

# Clean data ====
erasmus_data <-
  erasmus %>%
  filter(
    activity_mob !="Youth Exchanges - Programme Countries"
    ) %>%
  filter(
    participant_gender != "Undefined"
    ) %>%
  filter(
    participant_age > 11 & participant_age < 26
    )

# Create data to plot
erasmus_plot <- 
  erasmus_data %>%
  mutate(
    sending_country = 
      countrycode(
        sending_country_code, 
        origin = "iso2c",
        destination = "country.name")
         ) %>%
  select(
    academic_year, participant_gender, sending_country, sending_country_code
    ) %>%
  filter(
    sending_country %in% c("Belgium", "Netherlands", "Luxembourg")
  ) %>%
  group_by(
    academic_year, participant_gender, sending_country
    ) %>%
  count() %>%
  mutate(year = factor(academic_year),
         gender = factor(participant_gender,
                         levels = c("Female", "Male"),
                         ordered = TRUE)) %>%
  ungroup()

# Create dumbell plot on Belgium ====
dumbell_BE <- erasmus_plot %>%
  filter(sending_country == "Belgium") %>%
  mutate(paired = rep(1:6, each=2)) %>%
  ggplot(aes(x = n, y = fct_rev(year)))+
  geom_line(aes(group = paired), size = .5) +
  geom_point(aes(color = gender), size = 3, alpha = .75) +
  scale_x_continuous(breaks = seq(0,250,50), limits = c(0, 278), position = "top") +
  scale_y_discrete(expand = c(0,.2)) +
  scale_color_manual(values = c("#F8B7CD", "#67A3D9")) +
  theme_minimal() +
  theme(text = element_text(family = "Lato"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(color = "grey50"),
        strip.placement = "inside",
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "cm"),
        panel.grid.major.x = element_line(size = .2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Create dumbell plot on Netherlands ====
dumbell_NET <- erasmus_plot %>%
  filter(sending_country == "Netherlands") %>%
  mutate(paired = rep(1:6, each=2)) %>%
  ggplot(aes(x = n, y = fct_rev(year)))+
  geom_line(aes(group = paired), size = .5) +
  geom_point(aes(color = gender), size = 3, alpha = .75) +
  scale_x_continuous(breaks = seq(0,250,50), limits = c(0, 278), position = "top") +
  scale_y_discrete(expand = c(0,.2)) +
  scale_color_manual(values = c("#F8B7CD", "#67A3D9")) +
  theme_minimal() +
  theme(text = element_text(family = "Lato"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        strip.placement = "inside",
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "cm"),
        panel.grid.major.x = element_line(size = .2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Create dumbell plot on Luxembourg ====
dumbell_LUX <- erasmus_plot %>%
  filter(sending_country == "Luxembourg") %>%
  mutate(paired = rep(1:6, each=2)) %>%
  ggplot(aes(x = n, y = fct_rev(year)))+
  geom_line(aes(group = paired), size = .5) +
  geom_point(aes(color = gender), size = 3, alpha = .75) +
  scale_x_continuous(breaks = seq(0,250,50), limits = c(0,278), position = "top") +
  scale_y_discrete(expand = c(0,.2)) +
  scale_color_manual(values = c("#F8B7CD", "#67A3D9")) +
  theme_minimal() +
  theme(text = element_text(family = "Lato"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        strip.placement = "inside",
        strip.text = element_blank(),
        panel.spacing.x = unit(2, "cm"),
        panel.grid.major.x = element_line(size = .2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Create Belgian flag ====
flag_BE <- ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 92.5, ymin = 0, ymax = .6),
    fill = "#000000", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 92.6, xmax = 185, ymin = 0, ymax = .6),
    fill = "#FDDA24", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 185.1, xmax = 277.5, ymin = 0, ymax = .6),
    fill = "#EF3340", alpha = .75
  )  +
  theme_void()

# Create flag of Netherlands====
flag_NET <- ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 277.5, ymin = 0, ymax = .2),
    fill = "#003DA5", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 0, xmax = 277.5, ymin = .21, ymax = .4),
    fill = "#FFFFFF", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 0, xmax = 277.5, ymin = .41, ymax = .6),
    fill = "#C8102E", alpha = .75
  ) +
  theme_void()

# Create flag of Luxembourg====
flag_LUX <- ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 92.5, ymin = 0, ymax = .6),
    fill = "#EF3340", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 92.6, xmax = 185, ymin = 0, ymax = .6),
    fill = "#FFFFFF", alpha = .75
  ) + 
  geom_rect(
    aes(xmin = 185.1, xmax = 277.5, ymin = 0, ymax = .6),
    fill = "#00A2E1", alpha = .75
  )  +
  theme_void()


# Assemble plot ====
((plot_spacer() + flag_BE + plot_spacer() + 
    plot_spacer() + flag_NET + plot_spacer() + 
    plot_spacer() + flag_LUX + plot_spacer()) +
   plot_layout(nrow = 1,  widths = c(.1, .15, .1, .1, .15, .1, .1, .15, .1))) /
  (plot_spacer() + plot_spacer() + plot_spacer()) /
  ((dumbell_BE + plot_spacer() + dumbell_NET + plot_spacer() + dumbell_LUX) + 
   plot_layout(nrow = 1, widths = c(.8, .1, .8, .1, .8))) +
  plot_layout(nrow = 3, heights = c(.1, .1, 1)) +
  plot_annotation(title = "Comparison of the number of <span style='color:#F8B7CD;'><b>female</b></span style> and <span style='color:#67A3D9;'><b>male</b></span style> students in Belgium, the Netherlands and Luxemburg who participated in an Erasmus program between the academic years 2014-2015 and 2019-22020.") &
  theme(plot.margin = margin(0,0,0,0, "cm"),
        plot.title = element_textbox_simple(padding  = margin(3,3,3,3),
                                            margin = margin(0,0,.5,0,"cm"),
                                            size = 14,
                                            r = unit(6,"pt")))

# Save plot ====
pdf_file <- here("Figures", "2022-10 Erasmus.pdf")

ggsave(
  pdf_file,
  device = cairo_pdf,
  width = 22, 
  height = 8.42,
  unit = "cm"
)

rm(list = ls())

