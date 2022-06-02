# Week 18 (2022): Energy

# Load packages ====
library(tidyverse)
library(ggforce)      
library(ggtext)       
library(geomtextpath) 
library(patchwork)    

# Load data ====
capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')

# Get overview of data ====
skimr::skim(capacity)

# Summary of evolution in total gigawatts produced by power
capacity %>%
  select(type, year, total_gw) %>%
  pivot_wider(id_cols = year, names_from = type, values_from = total_gw) 

# Tidy and prepare data for plotting ====
types <- 
  capacity %>% 
  select(type, year, total_gw) %>%
  mutate(power = factor(type, 
                        levels = c("Coal", "Nuclear", "Other", 
                                   "Gas", "Storage", "Wind", "Solar"),
                        ordered = TRUE),
         #' Variable for coloring lines
         color = case_when(type == "Coal" ~ "Coal",
                           type == "Nuclear" ~ "Nuclear",
                           type == "Solar" ~"Solar",
                           TRUE ~ " "),
         #' Variable for labeling lines
         text = case_when(type == "Wind" ~ "Wind",
                          type == "Gas" ~ "Gas",
                          type == "Storage" ~ "Storage",
                          type == "Other" ~ "Other",
                          TRUE ~ " "))

# Create plots on evolution of total GW produced over time by resource ====
Plot_1 <- 
  types %>%
  ggplot(aes(x = year, y = total_gw, group = power)) +
  geom_line(aes(color = color), alpha = .75, size = .5) +
  scale_x_continuous(breaks = seq(2014, 2020, 1), 
                     name = "Year",
                     position = "top",
                     expand = c(0.01,0.01)) +
  scale_color_manual(values = c("grey80", "#4E3524", "red", "darkorange")) +
  labs(title="<span style='font-size:16pt;'><b>Total gigawatt of energy produced by power</b></span style><br><span style='font-size:9pt;'>The trends for <span style='color:#4E3524;'><b>coal</b></span style> and <span style='color:red;'><b>nuclear</b></span style> are hard to discern due to their large difference in range compared to that of the other powers (especially that of <span style='color:darkorange;'><b>solar energy</b></span style>)<sup>*</sup>.<br>") +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(text = element_text(family = "Charter"),
        plot.title = element_textbox_simple(lineheight = 1),
        plot.title.position = "plot",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 7, color = "grey80"),
        panel.grid.major.x = element_line(size = .05, color = "black", linetype = "longdash"),
        panel.grid.major.y = element_line(size = .075, color = "grey85"),
        panel.grid.minor = element_blank(),
        legend.position  = "none")

Plot_2 <- types %>%
  ggplot(aes(x = year, y = total_gw, group = power)) +
  geom_textline(aes(color = color, label = power), linewidth = .5, alpha = .75, size = 2.5, hjust = 0, vjust = -.3) +
  facet_zoom(y = power %in% c("Nuclear", "Coal"), zoom.data = power %in% c("Nuclear", "Coal"), horizontal = TRUE, split = TRUE,  zoom.size = .5) +
  scale_x_continuous(breaks = seq(2014, 2020, 1), name = "Year", position = "top", expand = c(0.01,0.01)) +
  scale_color_manual(values = c("grey80", "#4E3524", "red", "darkorange")) +
  labs(title = "<br><span style='font-size:9pt;'>Zooming in on the trend lines for coal and nuclear power, while the lines for solar energy, wind energy, gas and others are shown in the main plot.<br>",
       caption = "<span style='color:grey60;'><sup>*</sup>Ranges are <span style='color:darkorange;'>44.6 - 462.4</span style>, <span style='color:red;'>0.82 - 10</span style> and <span style='color:#4E3524;'>0.05 - 3.66</span style><br><span style='color=grey80;'>Data source: Berkely Lab (emp.lbl.gov/utility-scale-solar) | #tidytuesday | @TinevanDaal</span style>") +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(text = element_text(family = "Charter"),
        plot.title = element_textbox_simple(lineheight = .9),
        plot.title.position = "plot",
        plot.caption = element_markdown(size = 6, hjust = 0),
        plot.caption.position = "plot",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 7, color = "grey80"),
        strip.background = element_rect(fill = "grey95", color = "white"),
        panel.grid.major.x = element_line(size = .05, color = "black", linetype = "longdash"),
        panel.grid.major.y = element_line(size = .075, color = "grey85"),
        panel.grid.minor = element_blank(),
        legend.position  = "none")

# Assemble plots ====

Power_plot <- Plot_1 / Plot_2 

# Save plot ===
png_file <- here::here("Figures", "2022-18 Energy.png")

ggsave(
  png_file,
  device = png,
  width = 12.2, 
  height = 6.49, 
  units  = "in"
)

# Clean workspace
rm(list = ls())



