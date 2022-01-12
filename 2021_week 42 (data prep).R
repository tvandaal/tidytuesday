# Week 42 (2021): Growth/decline in number of nurses employed

library(tidyverse)    # data manipulation
library(tidytuesdayR) # load tidytuesday data

# Load tidy tuesday data
Loaded    <- tt_load("2021-10-12")

# Check (number of) countries in each dataset
country_fish <- map(Loaded, function(x){unique(x$Code)})
map(country_fish, unique)
map(country_fish, length)

# Merge data on fish production and consumption
Fish_production <- 
  left_join(Loaded$`capture-fisheries-vs-aquaculture`, Loaded$`capture-fishery-production`) %>%
  filter(!is.na(Code)) %>%
  mutate(Code      = ifelse(Entity == "Channel Islands",
                            yes = "GBR",
                            no  = Code),
         Continent = ifelse(str_count(Code) < 4 ,
                            yes = countrycode::countrycode(Code,
                                                           origin = "iso3c",
                                                           destination = "continent"),
                            no = "World"))

Fish_consumption <- 
  Loaded$`fish-and-seafood-consumption-per-capita` %>%
  filter(!is.na(Code)) %>%
  mutate(Code      = ifelse(Entity == "Channel Islands",
                            yes = "GBR",
                            no  = Code),
         Code      = ifelse(Entity == "Netherlands Antilles",
                            yes = "NLD",
                            no  = Code),
         Continent = ifelse(str_count(Code) < 4 ,
                            yes = countrycode::countrycode(Code,
                                                           origin = "iso3c",
                                                           destination = "continent"),
                            no = "World"))

Fish <- left_join(Fish_production, Fish_consumption) %>% arrange(Year)

rm(Fish_consumption, Fish_production, Loaded, country_fish)

# Calculate production and consumption per continent and for the world
Production_Continent <-
  Fish %>%
  select(c(3:6)) %>%
  group_by(Year, Continent) %>%
  summarise(Aqua_sum = sum(`Aquaculture production (metric tons)`, na.rm = TRUE),
            Wild_sum = sum(`Capture fisheries production (metric tons)`, na.rm = TRUE))

Production_World <-
  Production_Continent %>%
  group_by(Year) %>%
  summarise(Aqua_sum = sum(Aqua_sum, na.rm = TRUE),
            Wild_sum = sum(Wild_sum, na.rm = TRUE))

Consumption_Continent <-
  Fish %>%
  select(c(3, 6:7)) %>%
  group_by(Year, Continent) %>%
  summarise(Consumption_avg = mean(`Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`, na.rm = TRUE)) %>%
  filter(!is.nan(Consumption_avg))

Consumption_World <-
  Consumption_Continent %>%
  group_by(Year) %>%
  summarise(Consumption_avg = mean(Consumption_avg, na.rm = TRUE)) %>%
  filter(!is.nan(Consumption_avg))

save(Production_World,  Production_Continent,
     Consumption_World, Consumption_Continent,
     file = here::here("Data", "2021-42 Fish.RData"))

# Clean workspace
rm(list = ls())