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

# Select the year (2024) and the relevant variables ("Solar", "Wind and solar")
solar_country_2024 <- data_long %>%
  filter(
    Year == 2024,
    Variable %in% c(
      "Solar",
      "wind and solar"
    )
  )

# Convert Value to numeric
solar_country_2024$Value <- as.numeric(solar_country_2024$Value)

# ---- TOP 10 Countries by Solar (TWh) ---- #
top10_solar_country_TWh <- solar_country_2024 %>%
  filter(Unit == "TWh") %>%
  group_by(Area) %>%
  summarise(Total_Solar_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Solar_TWh)) %>%
  slice_head(n = 10)

ggplot(
  top10_solar_country_TWh, 
  aes(x = reorder(Area, Total_Solar_TWh), 
      y = Total_Solar_TWh, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Solar Energy Generation (TWh, 2024)",
    x = NULL,
    y = "Solar Energy Generation (TWH)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ---- TOP 10 Countries by Solar (%) ---- #
top10_solar_country_percent <- solar_country_2024 %>%
  filter(Unit == "%") %>%
  group_by(Area) %>%
  summarise(Total_Solar_Percent = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Solar_Percent)) %>%
  slice_head(n = 10)

ggplot(
  top10_solar_country_percent, 
  aes(x = reorder(Area, Total_Solar_Percent), 
      y = Total_Solar_Percent, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Solar Energy Share (% of total, 2024)",
    x = NULL,
    y = "Solar Energy Share (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
