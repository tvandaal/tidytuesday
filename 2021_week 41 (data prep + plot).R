# Week 41 (2021): Growth/decline in number of nurses employed

library(tidyverse)    # data manipulation
library(tidytuesdayR) # load tidytuesday data
library(ggtext)       # integrate html code in text

# Load data
Loaded    <- tt_load("2021-10-05")
Tidy_data <- Loaded$nurses

rm(Loaded)

# Subset data and arrange by year
Nurses_state <-
  Tidy_data %>%
  select(c(1:3)) %>%
  rename(N_nurses = `Total Employed RN`) %>%
  arrange(Year)

# Calculate relative growth in number of nurses per state
Nurses_growth <- 
  Nurses_state %>%
  group_by(State) %>%
  mutate(Difference = N_nurses - lag(N_nurses),
         Percentage = round((Difference / lag(N_nurses))*100, digits = 2),
         Growth     = factor(ifelse(Percentage > 0, yes = "green4", no = "#FF3333")))
save(Nurses_growth, file = here::here("Data", "2021-41 Nurses.RData"))

# Line plot of relative growth per state
Nurses_plot <-
  ggplot(Nurses_growth,
       aes(x = Year, y = Percentage, group = State)) +
  geom_rect(xmin = 1998,    xmax = 2000, ymin = -32, ymax = 104, 
            color = "#f4f0ec", fill  = "#f4f0ec", alpha = 0.5) +
  geom_rect(xmin = 2000.01, xmax = 2005, ymin = -32, ymax = 104,
            color = "#e5e4e2", fill  = "#e5e4e2") +
  geom_rect(xmin = 2005.01, xmax = 2010, ymin = -32, ymax = 104, 
            color = "#f4f0ec", fill  = "#f4f0ec", alpha = 0.5) +
  geom_rect(xmin = 2010.01, xmax = 2015, ymin = -32, ymax = 104, 
            color = "#e5e4e2", fill  = "#e5e4e2", alpha = 0.5) +
  geom_rect(xmin = 2015.01, xmax = 2020, ymin = -32, ymax = 104, 
            color = "#f4f0ec", fill  = "#f4f0ec", alpha = 0.5) +
  geom_segment(x=1998, xend = 2020, y = 0, yend = 0, 
               color = "black", size = .1, linetype = "dashed") + 
  ggforce::geom_link2(aes(color = after_stat(y < 0)), size = 0.5) +
  scale_colour_manual(values = c("green4", "#BF0A30")) + 
  facet_wrap(~State) +
  labs(title = "<span style = 'font-size:14pt;'><b>Relative <span style='color:green4;'>growth</span style> and <span style = 'color:#BF0A30;'>decline</span style> of the number of
       nurses employed between 1998 and 2020</b></span style><span style = 'font-size:8pt'><br><em>Calculated for each state as a percentage that expresses the difference in number of nurses employed within two consecutive years</span style>",
       caption = "<br><br>Data source: Data.World | #tidytuesday | @TinevanDaal") +

  scale_y_continuous(expand = c(0,0)) + 
  theme_minimal() +
  theme(text                  = element_text(family = "EB Garamond"),
        plot.title            = element_textbox_simple(lineheight = .8,
                                                       padding    = margin(5, 5, 5, 5), 
                                                       margin     = margin(0, 0, 10, 0)),
        plot.caption          = element_textbox_simple(color = "grey80", size = 6, hjust = 1),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        axis.title            = element_blank(),  
        axis.text.x           = element_text(size = 6, color = "grey60"),
        axis.text.y           = element_text(size = 6, color = "grey60"),
        strip.text            = element_text(face = "plain", size = 8), 
        panel.grid            = element_blank(),
        legend.position       = "none")
Nurses_plot

# Save plot
ggsave(Nurses_plot, 
       file = here::here("Figures", "2021-41 Nurses.png"),
       width = 25, 
       height = 20, 
       unit = "cm")  

# Clean workspace
rm(list = ls())

