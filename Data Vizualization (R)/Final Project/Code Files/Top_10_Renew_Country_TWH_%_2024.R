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

# Select the year (2024) and the relevant variables ("Hydro, bioenergy and other renewables", "Renewables", "Other renewables")
renewables_country_2024 <- data_long %>%
  filter(
    Year == 2024,
    Variable %in% c(
      "Hydro, bioenergy and other renewables",
      "Renewables",
      "Other renewables"
    )
  )

# Convert Value to numeric
renewables_country_2024$Value <- as.numeric(renewables_country_2024$Value)

# ---- TOP 10 Countries by Renewables (TWh) ---- #
top10_renew_country_TWh <- renewables_country_2024 %>%
  filter(Unit == "TWh") %>%
  group_by(Area) %>%
  summarise(Total_Renewables_TWh = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Renewables_TWh)) %>%
  slice_head(n = 10)

ggplot(
  top10_renew_country_TWh, 
  aes(x = reorder(Area, Total_Renewables_TWh), 
      y = Total_Renewables_TWh, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Renewables Energy Generation (TWh, 2024)",
    x = NULL,
    y = "Renewable Energy Generation (TWH)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 10))


# ---- TOP 10 Countries by Renewables Energy Share(%) ---- #
top10_renew_country_percent <- renewables_country_2024 %>%
  filter(Unit == "%") %>%
  group_by(Area) %>%
  summarise(Total_Renewables_Percent = median(Value, na.rm = TRUE)) %>%
  arrange(desc(Total_Renewables_Percent)) %>%
  slice_head(n = 10)

ggplot(
  top10_renew_country_percent, 
  aes(x = reorder(Area, Total_Renewables_Percent), 
      y = Total_Renewables_Percent, fill = Area)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "TOP 10 Countries by Renewables Energy Share (% of total, 2024)",
    x = NULL,
    y = "Renewable Energy Share (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 10))
