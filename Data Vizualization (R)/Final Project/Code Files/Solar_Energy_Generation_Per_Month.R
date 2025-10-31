# Libraries
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Reading the data
energy_generation <- read_csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/merged_electricity generation_actual.csv")

# Change the date's format
energy_generation <- energy_generation %>%
  mutate(
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    end_date   = as.Date(end_date,   format = "%m/%d/%Y")
  )

df_month <- energy_generation %>%
  mutate(month = as.Date(cut(start_date, "month"))) %>%
  group_by(month) %>%
  summarise(
    solar_generation = sum(`Photovoltaics (MWh)`, na.rm = TRUE)
  )

ggplot(df_month, aes(x = month, y = solar_generation)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Solar Energy Generation in Germany",
    x = "Month",
    y = "Electricity Generation (MWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


################

# Wind generation

df_wind <- energy_generation %>%
  mutate(month = as.Date(cut(start_date, "month"))) %>%
  group_by(month) %>%
  summarise(
    wind_generation = sum(`Wind onshore (MWh)`, `Wind offshore (MWh)`, na.rm = TRUE)
  )

ggplot(df_wind, aes(x = month, y = wind_generation)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "Monthly Wind Energy Generation in Germany",
    x = "Month",
    y = "Electricity Generation (MWh)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
