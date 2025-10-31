# Libraries
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)

# Reading the dataset
data <- read.csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/merged_electricity generation_actual.csv")

# Filling the missing values with 0
data[is.na(data)] <- 0

# Convert dates into proper format
data <- data %>%
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    end_date = as.Date(end_date, format = "%m/%d/%Y"),
    year = year(start_date)
  )

# Calculating the sum of each generation type per years
yearly_generation_by_type <- data %>%
  group_by(year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

# Plotting the developing of each generation type by year
develope_generation_yearly <- yearly_generation_by_type %>%
  pivot_longer(
    cols = -c(year),
    names_to = "generation_type",
    values_to = "value"
  )

ggplot(develope_generation_yearly, aes(x = year, y = value, color = generation_type)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Germany: Electricity Generation by Energy Type (Yearly, 2015 - 2024)",
    x = "Year",
    y = "Electricity Generation (TWh)",
    color = "Generation Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
