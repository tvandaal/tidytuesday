# Week 20 (2022): Eurovision

# Load packages ====
library(tidyverse)
library(ggtext) 

# Load data ====
tuesdata <- tidytuesdayR::tt_load('2022-05-17')

eurovision <- tuesdata$eurovision
votes <- tuesdata$`eurovision-votes`

# Get overview of data ====
glimpse(votes)
skimr::skim(votes)

glimpse(eurovision)
skimr::skim(eurovision)

# Tidy data on votes ====
tidy_votes <-
  votes %>%
  #' Remove impossible rows (votes from country to that same country)
  filter(is.na(duplicate) == TRUE) %>%
  #' Only select data of finals 
  filter(!is.na(semi_final)) %>%
  mutate(
    #' Replace 'Netherlands' with 'The Netherlands'
    from_country = recode(from_country, Netherlands = "The Netherlands"),
    to_country = recode(to_country, Netherlands = "The Netherlands"),
    #' Create variable 'final' (1=final, 0=not final)
    final = ifelse(semi_final == "FALSE", yes = 1, no = 0)) %>%
  select(-c(duplicate, semi_final)) 

# Filter winners out of eurovision-data ====
winners <- eurovision %>%
  filter(winner == TRUE & section %in% c("grand-final", "final")) %>%
  select(year, winner = artist_country)

# Prepare votes-data for plotting ==== 
plot_votes <- 
  tidy_votes %>%
  #' Add information on winners
  left_join(winners, by = "year") %>%
  mutate(country = factor(from_country,
                          levels = unique(tidy_votes$from_country),
                          ordered = TRUE)) %>%
  select(year, country, winner) %>%
  distinct() %>%
  mutate(winner_D = ifelse(country == winner, yes = 1, no = 0)) %>%
  group_by(country) %>%
  mutate(N_wins = sum(winner_D),
         N_participations = n(),
         efficiency = round(N_wins/N_participations * 100, digits = 1),
         efficiency2 = paste(N_wins, N_participations, sep = "/")) %>%
  ungroup()

# Which countries got into the finals over the years?
finals_by_year <- 
  plot_votes %>%
  mutate(x = rep(2024, nrow(.))) %>%
  ggplot(aes(x = year, y = fct_rev(fct_infreq(country)))) + 
  geom_tile(aes(fill = factor(winner_D), color = factor(winner_D)), width = .9, alpha = .8) +
  geom_text(aes(x = x, y = fct_rev(fct_infreq(country)), label = efficiency2), 
                hjust = 1, size = 1.5, fontface = "bold", family = "Lato") + 
  scale_x_continuous(position = "top",
                     breaks = seq(1975, 2020, 5), 
                     name = NULL,
                     expand = c(0,0))+
  scale_y_discrete(expand = c(0,0)) + 
  scale_fill_manual(values = c("#ffd700", "#0057b7")) +
  scale_color_manual(values = c("#ffd700", "#0057b7")) +
  labs(title = "<span style = 'font-size=16pt'>Spain and the United Kingdom always <span style='color:#ffd700'><b>made it to the final</b></span style> of the Eurovision songfestival. Sweden, France, Norway and Germany missed it only once. Ukraine is the most <b><i>efficient</i></b> by <span style='color:#0057b7'><b>winning</b></span style> the final 3 out of 17 times.</span style>") +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(text = element_text(family = "Lato"),
        plot.title = element_textbox_simple(lineheight = .9, 
                                            fill = "white",
                                            padding = margin(2,2,2,2,"mm"),
                                            margin = margin(0,0,2,0,"mm")),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text.x  = element_text(size = 7),
        axis.text.y  = element_text(size = 5),
        axis.title = element_blank(),
        axis.ticks.x.top = element_line(size = .2, color = "grey50"),
        axis.ticks.y = element_line(size = .1, color = "grey90"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "grey90"),
        panel.grid.major.y = element_line(size = .1, color = "grey90"),
        legend.position = "none")
finals_by_year

# Save plot ===
png_file <- here::here("Figures", "2022-20 Eurovision.png")

ggsave(
  png_file,
  device = png,
  width = 10.7, 
  height = 6.49, 
  units  = "in",
  dpi = 320
)

# Clean workspace
rm(list = ls())



