# Libraries
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Reading the data
energy_generation_one_day <- read_csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/merged_electricity generation_actual.csv")

# Change the date's format
energy_generation_one_day <- energy_generation_one_day %>%
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    end_date   = as.Date(end_date,   format = "%m/%d/%Y")
  )

# Selecting a concrete date
selected_date <- energy_generation_one_day %>%
  filter(start_date == as.Date("2017-12-30") & end_date == as.Date("2017-12-31"))

# Changing into long format for the visualization
long_date <- selected_date %>%
  select(-start_date, -end_date, -date) %>%
  pivot_longer(
    everything(),
    names_to = "generation_type",
    values_to = "value"
  )

# Bar chart
ggplot(long_date, aes(x = reorder(generation_type, value), y = value, fill = generation_type)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Electricity Generation by Source for 2017-12-31",
    x = "Generation Type",
    y = "Electricity Generation (TWh)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 10)
  )
