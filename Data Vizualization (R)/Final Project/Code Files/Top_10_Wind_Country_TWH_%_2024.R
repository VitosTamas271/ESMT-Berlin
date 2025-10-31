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

# Select the year (2024) and the relevant variables ("Wind", "Wind and solar", "Onshore wind)
wind_country_2024 <- data_long %>%
  filter(
    Year == 2024,
    Variable %in% c(
      "Wind",
      "wind and solar",
      "Onshore wind"
    )
  )

# Convert Value to numeric
wind_country_2024$Value <- as.numeric(wind_country_2024$Value)

# ---- TOP 10 Countries by Wind (TWh) ---- #
top10_wind_country_TWh <- wind_country_2024 %>%
  filter(Unit == "TWh") %>%
  group_by(Area) %>%
  summarise(Total_Wind_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Wind_TWh)) %>%
  slice_head(n = 10)

ggplot(
  top10_wind_country_TWh, 
  aes(x = reorder(Area, Total_Wind_TWh), 
      y = Total_Wind_TWh, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Wind Energy Generation (TWh, 2024)",
    x = NULL,
    y = "Wind Energy Generation (TWH)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ---- TOP 10 Countries by Wind (%) ---- #
top10_wind_country_percent <- wind_country_2024 %>%
  filter(Unit == "%") %>%
  group_by(Area) %>%
  summarise(Total_Wind_Percent = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Wind_Percent)) %>%
  slice_head(n = 10)

ggplot(
  top10_wind_country_percent, 
  aes(x = reorder(Area, Total_Wind_Percent), 
      y = Total_Wind_Percent, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Wind Energy Share (% of total, 2024)",
    x = NULL,
    y = "Wind Energy Share (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
