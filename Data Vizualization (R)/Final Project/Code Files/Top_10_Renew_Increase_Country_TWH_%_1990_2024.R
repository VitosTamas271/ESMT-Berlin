# Libraries
library(dplyr)
library(readr)
library(tidyverse)

# Reading the dataset
data <- read.csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/europe_yearly_full_release_reshaped.csv")

data_long <- data %>%
  pivot_longer(
    cols = matches("^Variable|Value|Unit"),
    names_to = c(".value", "set"),
    names_pattern = "(Variable|Value|Unit)\\.?([0-9]*)"
  )

# Select the year (1990-2024) and the relevant variables ("Hydro, bioenergy and other renewables", "Renewables", "Other renewables")
renewables_country_1990_2024_twh <- data_long %>%
  filter(
    Year >= 1990 & Year <= 2024,
    Variable %in% c(
      "Hydro, bioenergy and other renewables",
      "Renewables",
      "Other renewables"
    ),
    Unit == "TWh"
  )

renewables_country_1990_2024_percent <- data_long %>%
  filter(
    Year >= 1990 & Year <= 2024,
    Variable %in% c(
      "Hydro, bioenergy and other renewables",
      "Renewables",
      "Other renewables"
    ),
    Unit == "%"
  )

# Convert Value to numeric
renewables_country_1990_2024_twh$Value <- as.numeric(renewables_country_1990_2024_twh$Value)
renewables_country_1990_2024_percent$Value <- as.numeric(renewables_country_1990_2024_percent$Value)

# ---- TOP 10 Countries by increasing the Renewables (TWh) in 1990 - 2024 ---- #

# Defining the TOP 10 countries to 2024
top10_renew_country_2024_TWh <- renewables_country_1990_2024_twh %>%
  filter(Year == 2024) %>%
  group_by(Area) %>%
  summarise(Total_Renewables_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Renewables_TWh)) %>%
  slice_head(n = 10) %>%
  pull(Area)

# The data for the TOP 10 country for each year
top10_renew_country_each_year_twh <- renewables_country_1990_2024_twh %>%
  filter(Area %in% top10_renew_country_2024_TWh) %>%
  group_by(Area, Year) %>%
  summarise(Total_Renewables_TWh = sum(Value, na.rm = TRUE), .groups = "drop")

# Line chart about the increase between 1990 - 2024
ggplot(
  top10_renew_country_each_year_twh, aes(x = Year, y = Total_Renewables_TWh, color = Area)
  ) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Renewable Energy Generation (TWh) Over the Years (1990 - 2024)",
    x = "Year",
    y = "Renewable Energy Generation (TWh)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
    )

# ---- TOP 10 Countries by increasing the Renewables Energy Shares (%) in 1990 - 2024 ---- #

# Defining the TOP 10 countries to 2024
top10_renew_country_2024_percent <- renewables_country_1990_2024_percent %>%
  filter(Year == 2024) %>%
  group_by(Area) %>%
  summarise(Total_Renewables_TWh = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Renewables_TWh)) %>%
  slice_head(n = 10) %>%
  pull(Area)

# The data for the TOP 10 country for each year
top10_renew_country_each_year_percent <- renewables_country_1990_2024_percent %>%
  filter(Area %in% top10_renew_country_2024_percent) %>%
  group_by(Area, Year) %>%
  summarise(Total_Renewables_Percent = median(Value, na.rm = TRUE), .groups = "drop")

# Line chart about the increase between 1990 - 2024
ggplot(
  top10_renew_country_each_year_percent, aes(x = Year, y = Total_Renewables_Percent, color = Area)
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Renewable Energy Shares (%) Over the Years (1990 - 2024)",
    x = "Year",
    y = "Renewable Energy Shares (%)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
  )
